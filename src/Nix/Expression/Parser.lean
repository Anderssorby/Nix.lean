import Lean.Parser
import Nix.Parsec
import Nix.Expression.Types

open Std (RBNode RBNode.singleton RBNode.leaf)

namespace Nix.Expression

namespace Parser

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

@[inline]
def digitNonZero : Parsec Nat := do
  let c ← satisfy (fun c => '1' ≤ c ∧ c ≤ '9') -- "1-9"
  c.val.toNat - '0'.val.toNat

@[inline]
def digitNat : Parsec Nat := do
  let c ← satisfy (fun c => '0' ≤ c ∧ c ≤ '9')
  c.val.toNat - '0'.val.toNat

def digitsToNat (ds : Array Nat) : Nat :=
  ds.toList.enum.foldl (λ acc (d, i) => acc + d * 10 ^ i) 0

def num : Parsec Number := do
  let sign : Int ←
    (do skipChar '-'; pure (-1 : Int))
      <|> pure 1
  let digits ← many1 (digitNat)
  let whole := sign * digitsToNat digits
  let optDecimals : Option Number ← option do
    skipChar '.'
    let digits ← many1 digitNat
    let exponent := digits.size
    let mantissa := whole * (10 ^ exponent : Nat) + sign * digitsToNat digits
    { mantissa, exponent : Number }
  let numb : Number :=  optDecimals.getD <| Number.fromInt whole
  let withExp : Parsec Number := do
    skipChar 'e' <|> skipChar 'E'
    let sign ← pchar '-' <|> pchar '+'
    let n ← digitsToNat <$> many1 digitNat
    if sign = '-' then 
      numb.shiftr n
    else
      numb.shiftl n

  withExp <|> pure numb

def reservedWords := #[ "let", "with", "in", "if", "then", "else", "inherit", "rec" ]

@[inline]
def name : Parsec String := do
  let starters := List.foldl (λ p c => p <|> pchar c) asciiLetter ['_', '$']
  let c ← starters
  let rest ← manyChars (starters <|> digit)
  let n := s!"{c}{rest}"
  if Array.all reservedWords (λ s => s != n) then
    n
  else
    fail "reserved"

def fvar : Parsec Expr := map Expr.fvar name

def litteral : Parsec Expr := do
  let string := map Expr.str stringLitteral
  let number := do
      let n ← num
      ws
      Expr.num n
  string <|> number

def operator : Parsec Operator := do
  let dot := do
    skipChar '.'
    Operator.dot
  let merge := do
    skipString "//"
    Operator.merge
  let or := do
    skipString "||"
    Operator.or_
  let and := do
    skipString "&&"
    Operator.and_
  let append := do
    skipString "++"
    Operator.append
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

mutual
partial def list : Parsec Expr := do
  skipChar '['
  let l ← many expression
  skipChar ']'
  Expr.list l

partial def attrset : Parsec Expr := do
  let isRec ← test <| skipString "rec"
  ws
  skipChar '{'
  let rec internal : Parsec (RBNode String (fun _ => Expr)) := do
    ws
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
      ws
      let kvs ← internal
      kvs.insert compare k v
  Expr.attrset isRec (← internal)

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

partial def withStatement : Parsec Expr := do
  skipString "with"
  ws
  let w ← fvar
  ws
  skipChar ';'
  ws
  let e ← expression
  Expr.withStatement w e

/-
Parse a let statement
let <assignments>+ in
-/
partial def letStatement : Parsec Expr := do
  let assignment : Parsec (Name × Expr) := do
    ws
    let com ← option <| manyStrings comment
    ws
    let start ← getPos
    let n ← name
    ws
    skipChar '='
    ws
    let ex ← expression
    ws
    skipChar ';'
    let end_ ← getPos
    ws
    (n, Expr.meta {start, end_} com ex)
  skipString "let"
  ws
  let ass ← many1 assignment
  ws
  skipString "in"
  ws
  let ex ← expression
  Expr.letExpr ass ex

partial def lambda : Parsec Expr := do
  let part : Parsec (Name × Option Expr) := do
    ws
    let n ← name
    ws
    let optDef ← option <| do
      skipChar '?'
      ws
      expression
    ws
    (n, optDef)
  let destruct : Parsec (Array (Name × Option Expr) × Bool) := do
    skipChar '{'
    ws
    let bs ← many part
    ws
    let catchAll ← test <| skipString "..."
    ws
    skipChar '}'
    (bs, catchAll)
  let n ← name
  ws
  skipChar ':'
  ws
  let e ← expression
  Expr.lam n e

partial def comment : Parsec String := do
  ws
  skipChar '#'
  manyChars <| satisfy (λ c => c != '\n')

/-
Function application e1 e2
-/
partial def app : Parsec Expr := do
  let e1 ← recSafeExpression
  ws
  let e2 ← expression
  Expr.app e1 e2

/-
Binary operation e1 `op` e2
-/
partial def operation : Parsec Expr := do
  ws
  let e1 ← recSafeExpression
  ws
  let op ← operator
  ws
  let e2 ← expression
  Expr.opr e1 op e2

partial def recSafeExpression : Parsec Expr := do
  let wrappedExpression : Parsec Expr := do
    skipChar '('
    let e ← expression
    skipChar ')'
    e
  litteral
    <|> lambda
    <|> list
    <|> attrset
    <|> ifStatement
    <|> letStatement
    <|> fvar
    <|> wrappedExpression

partial def expression : Parsec Expr := do
  let com ← option <| manyStrings comment
  ws
  recSafeExpression
    <|> operation
    <|> app

end

def file : Parsec Expr := do
  ws
  let res ← expression
  eof
  res

end Parser

def parse (s : String) : Except String Nix.Expr :=
  match Parser.file { it := s.mkIterator : Parsec.Pos } with
  | Parsec.ParseResult.success _ res => Except.ok res
  | Parsec.ParseResult.error pos err  => Except.error s!"{err} ({pos.line}:{pos.lineOffset})"

end Nix.Expression
