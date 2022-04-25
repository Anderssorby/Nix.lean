import Parsec
import Nix.Expression.Types

open Std (RBNode RBNode.singleton RBNode.leaf)

namespace Nix.Expression

namespace Parser

open Parsec

@[inline]
def getPos : Parsec Position := do
  let s ← get
  return { line := s.line, lineOffset := s.lineOffset }

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
  | '\\' => pure '\\'
  | '"'  => pure '"'
  | '/'  => pure '/'
  | 'b'  => pure '\x08'
  | 'f'  => pure '\x0c'
  | 'n'  => pure '\n'
  | 'r'  => pure '\x0d'
  | 't'  => pure '\t'
  | 'u'  =>
    let u1 ← hexChar; let u2 ← hexChar; let u3 ← hexChar; let u4 ← hexChar
    return Char.ofNat $ 4096*u1 + 256*u2 + 16*u3 + u4
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
  let c ← satisfy (fun c => '1' ≤ c ∧ c ≤ '9')
  return c.val.toNat - '0'.val.toNat

@[inline]
def digitNat : Parsec Nat := do
  let c ← satisfy (fun c => '0' ≤ c ∧ c ≤ '9')
  return c.val.toNat - '0'.val.toNat

def digitsToNat (ds : Array Nat) : Nat :=
  ds.toList.reverse.enum.foldl (λ acc (i, d) => acc + d * 10 ^ i) 0

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
    return { mantissa, exponent : Number }
  let numb : Number :=  optDecimals.getD <| Number.fromInt whole
  let withExp : Parsec Number := do
    skipChar 'e' <|> skipChar 'E'
    let sign ← pchar '-' <|> pchar '+'
    let n ← digitsToNat <$> many1 digitNat
    if sign = '-' then 
      return numb.shiftr n
    else
      return numb.shiftl n

  withExp <|> pure numb

def reservedWords := #[ "let", "with", "in", "if", "then", "else", "inherit", "rec" ]

@[inline]
def name : Parsec Name := do
  let starters := List.foldl (λ p c => p <|> pchar c) asciiLetter ['_', '$', '-']
  let c ← starters
  let rest ← manyChars (starters <|> digit)
  let n := s!"{c}{rest}"
  if Array.all reservedWords (λ s => s != n) then
    return n
  else
    fail "reserved"

def fvar : Parsec Expr := ParsecM.map Expr.fvar name

def litteral : Parsec Expr := do
  let string := ParsecM.map Expr.str stringLitteral
  let number := do
      let n ← num
      ws
      return Expr.num n
  string <|> number

def operator : Parsec Operator := do
  let dot := do
    skipChar '.'
    return Operator.dot
  let merge := do
    skipString "//"
    return Operator.merge
  let or := do
    skipString "||"
    return Operator.or_
  let and := do
    skipString "&&"
    return Operator.and_
  let append := do
    skipString "++"
    return Operator.append
  let add := do
    skipChar '+'
    return Operator.add
  let minus := do
    skipChar '-'
    return Operator.minus
  let mul := do
    skipChar '*'
    return Operator.mul
  dot <|> merge <|> or <|> and <|> append <|> add <|> minus <|> mul

mutual

partial def stringInterpolation : Parsec Expr := do
  skipChar '$'
  skipChar '{'
  let e ← expression
  skipChar '}'
  return e

partial def attrSetKey : Parsec AttrSetKey := do
  let part := (Expr.fvar <$> name) <|> (Expr.str <$> stringLitteral) <|> stringInterpolation
  let first ← part
  let rest : Array Expr ← many do
    skipChar '.'
    part
  if rest.isEmpty then
    return AttrSetKey.name first
  else
    return AttrSetKey.expr (first :: rest.toList).toArray

partial def list : Parsec Expr := do
  skipChar '['
  let l ← many expression
  wsc
  skipChar ']'
  return Expr.list l

partial def attrset : Parsec Expr := do
  let isRec ← test <| skipString "rec"
  wsc
  skipChar '{'
  let rec internal : Parsec (List (AttrSetKey × Expr)) := do
    let com ← option <| manyStrings comment
    ws
    if (← test <| skipChar '}') then
      return []
    else
      let k ← attrSetKey
      wsc
      skipString "="
      wsc
      let start ← getPos
      let v ← expression
      let stop ← getPos
      wsc
      skipString ";"
      ws
      let kvs ← internal
      return kvs.append [(k, Expr.meta {start, stop} com v)]
  return Expr.attrset isRec (← internal)

partial def ifStatement : Parsec Expr := do
  skipString "if"
  ws
  let e ← expression
  wsc
  skipString "then"
  let t ← expression
  skipString "else"
  let f ← expression
  return Expr.ifStatement e t f

partial def withStatement : Parsec Expr := do
  skipString "with"
  wsc
  let w ← fvar
  wsc
  skipChar ';'
  ws
  let e ← expression
  return Expr.withStatement w e

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
    let stop ← getPos
    ws
    return (n, Expr.meta {start, stop} com ex)
  skipString "let"
  ws
  let ass ← many1 assignment
  wsc
  skipString "in"
  ws
  let ex ← expression
  return Expr.letExpr ass ex

partial def lambda : Parsec Expr := do
  let rec varsp : Parsec <| List (Name × Option Expr) × Bool := do
    wsc
    if ← test <| skipString "..." then
      return ([], true)
    let n ← name
    wsc
    let optDef ← option <| do
      skipChar '?'
      wsc
      Expr.num <$> num
      -- expression
    ws
    if ← test <| skipChar ',' then
      let (l, ca) ← varsp
      return ((n, optDef) :: l, ca)
    else
      return ([(n, optDef)], false)
  let destruct : Parsec LBinding := do
    let optName : Option Name ← option <| do
      let n ← name
      wsc
      skipChar '@'
      wsc
      return n
    skipChar '{'
    ws
    let (vars, catchAll) ← varsp
    ws
    skipChar '}'
    return LBinding.destructure optName vars.toArray catchAll
  let binding ← (LBinding.var <$> name) <|> destruct
  wsc
  skipChar ':'
  ws
  let e ← expression
  return Expr.lam binding e

partial def comment : Parsec String := do
  ws
  skipChar '#'
  let com ← manyChars <| satisfy (λ c => c != '\n')
  ws
  return com

partial def comments : Parsec (Option String) := option <| do
  manyStrings comment

partial def wsc : Parsec Unit := comments >>= (fun _ => pure ())

/-
Function application e1 e2
-/
partial def app : Parsec Expr := do
  let e1 ← recSafeExpression
  wsc
  let com ← comments
  let e2 ← expression
  return Expr.app e1 e2

/-
Binary operation e1 `op` e2
-/
partial def operation : Parsec Expr := do
  wsc
  let e1 ← recSafeExpression
  wsc
  let com ← comments
  wsc
  let op ← operator
  wsc
  let e2 ← expression
  return Expr.opr e1 op e2

partial def recSafeExpression : Parsec Expr := do
  let wrappedExpression : Parsec Expr := do
    skipChar '('
    let e ← expression
    skipChar ')'
    return e
  litteral
    <|> lambda
    <|> list
    <|> attrset
    <|> ifStatement
    <|> letStatement
    <|> fvar
    <|> stringInterpolation
    <|> wrappedExpression

partial def expression : Parsec Expr := do
  let com ← comments
  ws
  let start ← getPos
  let expr ←
    operation
    <|> app
    <|> recSafeExpression
  let stop ← getPos
  return Expr.meta {start, stop} com expr

end

def file : Parsec Expr := do
  ws
  let res ← expression
  wsc
  eof
  return res

end Parser

def parse (s : String) : Except String Nix.Expr := Parser.file.parse s

end Nix.Expression
