import Lean.Parser
import Nix.Parsec
import Nix.Expression

open Std (RBNode RBNode.singleton RBNode.leaf)

namespace Nix.Parser

open Parsec

@[inline]
def hexChar : Parsec Nat := do
  let c ← anyChar
  if '0' ≤ c ∧ c ≤ '9' then
    pure $ c.val.toNat - '0'.val.toNat
  else if 'a' ≤ c ∧ c ≤ 'f' then
    pure $ c.val.toNat - 'a'.val.toNat
  else if 'A' ≤ c ∧ c ≤ 'F' then
    pure $ c.val.toNat - 'A'.val.toNat
  else
    fail "invalid hex character"

def escapedChar : Parsec Char := do
  let c ← anyChar
  match c with
  | '\\' => '\\'
  | '"'  => '"'
  | '/'  => '/'
  | 'b'  => '\x08'
  | 'f'  => '\x0c'
  | 'n'  => '\n'
  | 'r'  => '\x0d'
  | 't'  => '\t'
  | 'u'  =>
    let u1 ← hexChar; let u2 ← hexChar; let u3 ← hexChar; let u4 ← hexChar
    Char.ofNat $ 4096*u1 + 256*u2 + 16*u3 + u4
  | _ => fail "illegal \\u escape"

partial def stringLitteral : Parsec String := do
  skipChar '"'
  let rec internal : String → Parsec String := λ acc => do 
    let c ← anyChar
    if c = '"' then
      return acc
    else
      if c = '\\' then
        internal <| acc.push (← escapedChar)
      -- as to whether c.val > 0xffff should be split up and encoded with multiple \u,
      -- the JSON standard is not definite: both directly printing the character
      -- and encoding it with multiple \u is allowed. we choose the former.
      else if 0x0020 ≤ c.val ∧ c.val ≤ 0x10ffff then
        internal <| acc.push c
      else
        fail "unexpected character in string"
  internal ""

partial def natCore (acc digits : Nat) : Parsec (Nat × Nat) := do
  let some c ← peek? | (acc, digits)
  if '0' ≤ c ∧ c ≤ '9' then
    skip
    let acc' := 10*acc + (c.val.toNat - '0'.val.toNat)
    natCore acc' (digits+1)
  else
    (acc, digits)

@[inline]
def lookahead (p : Char → Prop) (desc : String) [DecidablePred p] : Parsec Unit := do
  let c ← peek!
  if p c then
    ()
  else
    fail $ "expected " ++ desc

@[inline]
def natNonZero : Parsec Nat := do
  lookahead (fun c => '1' ≤ c ∧ c ≤ '9') "1-9"
  let (n, _) ← natCore 0 0
  n

@[inline]
def natNumDigits : Parsec (Nat × Nat) := do
  lookahead (fun c => '0' ≤ c ∧ c ≤ '9') "digit"
  natCore 0 0

@[inline]
def natMaybeZero : Parsec Nat := do
  let (n, _) ← natNumDigits
  n

def num : Parsec Number := do
  let c ← peek!
  let sign : Int ←
    if c = '-' then
      skip
      pure (-1 : Int)
    else
      pure 1
  let c ← peek!
  let res ←
    if c = '0' then
      skip
      pure 0
    else
      natNonZero
  let c? ← peek?
  let res : Number ←
    if c? = some '.' then
      skip
      let (n, d) ← natNumDigits
      if d > USize.size then fail "too many decimals"
      let mantissa' := sign * (res * (10^d : Nat) + n)
      let exponent' := d
      Number.mk mantissa' exponent'
    else
      Number.fromInt (sign * res)
  let c? ← peek?
  if c? = some 'e' ∨ c? = some 'E' then
    skip
    let c ← peek!
    if c = '-' then
      skip
      let n ← natMaybeZero
      res.shiftr n
    else
      if c = '+' then skip
      let n ← natMaybeZero
      if n > USize.size then fail "exp too large"
      res.shiftl n
  else
    res

def reservedWords := #[ "let", "with", "in", "if", "then", "else", "inherit", "rec" ]

@[inline]
def name : Parsec String := attempt do
  notFollowedBy <| Array.foldl (λ (p : Parsec Unit) s => p <|> skipString s) never reservedWords
  let starters := List.foldl (λ p c => p <|> pchar c) asciiLetter ['_', '$']
  let c ← starters
  let rest ← manyChars (starters <|> digit)
  return s!"{c}{rest}"

mutual
partial def list : Parsec Expr := do
  skipChar '['
  let l ← many expression
  skipChar ']'
  Expr.list l


partial def attrset : Parsec Expr := do
  skipChar '{'
  let rec internal : Parsec (RBNode String (fun _ => Expr)) := do
    if (← test <| skipChar '}') then
      RBNode.leaf
    else
      let k ← name <|> stringLitteral
      ws
      skipString "="
      ws
      let v ← expression
      ws
      skipString ";"
      let kvs ← internal
      kvs.insert compare k v
  Expr.attrset (← internal)

partial def litteral : Parsec Expr := do
  let string := map Expr.str stringLitteral
  let false := do
      skipString "false"
      ws
      Expr.bool false
  let true := do
      skipString "true" 
      ws
      Expr.bool true 
  let null := do
      skipString "null"
      ws
      Expr.null
  let number := do
      let n ← num
      ws
      Expr.num n
  string <|> false <|> true <|> null <|> map Expr.fvar name <|> number

partial def ifStatement : Parsec Expr := do
  skipString "if"
  ws
  let e ← expression
  ws
  skipString "then"
  let t ← expression
  skipString "else"
  let f ← expression
  Expr.ifStatement e t f

partial def letStatement : Parsec Expr := do
  let assignment : Parsec (Name × Expr) := do
    ws
    let n ← name
    ws
    skipChar '='
    ws
    let ex ← expression
    ws
    skipChar ';'
    ws
    (n, ex)
  skipString "let "
  ws
  let ass ← many1 assignment
  ws
  skipString "in "
  ws
  let ex ← expression
  Expr.letExpr ass ex

partial def lambda : Parsec Expr := do
  let n ← name
  ws
  skipChar ':'
  ws
  let e ← expression
  Expr.lam n e

/-
Function application e1 e2
-/
partial def app : Parsec Expr := do
  let e1 ← recSafeExpression
  ws
  let e2 ← expression
  Expr.app e1 e2

partial def operator : Parsec Operator := do
  let dot := do
    skipChar '.'
    Operator.dot
  let merge := do
    skipString "//"
    Operator.merge
  let or := do
    skipString "or"
    Operator.or
  let and := do
    skipString "and"
    Operator.and
  let append := do
    skipChar '+'
    Operator.add
  let add := do
    skipChar '+'
    Operator.add
  let minus := do
    skipChar '-'
    Operator.minus
  let mul := do
    skipChar '*'
    Operator.mul
  dot <|> merge <|> or <|> and <|> append <|> add <|> minus <|> mul


/-
Binary operation e1 `op` e2
-/
partial def operation : Parsec Expr := do
  let e1 ← recSafeExpression
  ws
  let op ← operator
  ws
  let e2 ← expression
  Expr.opr e1 op e2

partial def recSafeExpression : Parsec Expr := do
  litteral
    <|> list
    <|> attrset
    <|> ifStatement
    <|> letStatement
    <|> lambda

partial def expression : Parsec Expr := do
  let wrappedExpression : Parsec Expr := do
    skipChar '('
    let e ← expression
    skipChar ')'
    e
  ws
  recSafeExpression
    <|> operation
    <|> app
    <|> wrappedExpression

end

def any : Parsec Expr := do
  ws
  let res ← expression
  eof
  res

end Nix.Parser

namespace Nix

def parse (s : String) : Except String Nix.Expr :=
  match Nix.Parser.any s.mkIterator with
  | Parsec.ParseResult.success _ res => Except.ok res
  | Parsec.ParseResult.error it err  => Except.error s!"offset {it.i.repr}: {err}"

end Nix