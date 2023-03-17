import Megaparsec
import Nix.Expression.Types

-- open Std --(RBNode RBNode.singleton RBNode.leaf)

namespace Nix.Expression

namespace Parser

open Megaparsec Parsec Common

/-- We define `P` as a parsec over strings, whose tokens are chars -/
abbrev P := Parsec Char String Unit

-- /-- We start by parsing numbexrs, a sequence of at least some digit -/
-- def numP : P Expr := do
--   let x : List Char ← some' (satisfy Char.isDigit)
--   let str : String := String.mk x
--   return .num $ String.toNat! str

-- /-- A symbol must start with a letter, followed by any other alphanumeric character -/
-- def nameP : P Expr := do
--   let c : Char ← satisfy Char.isAlpha
--   let cs : List Char ← many' (satisfy Char.isAlphanum)
--   return .fvar $ ⟨c :: cs⟩

@[inline]
def nameP : P Name := do
  let starters := satisfy fun c => c.isAlpha || ['_', '$'].contains c
  let c ← starters
  let rest ← many' (starters <|> digit)
  let n := s!"{c}{rest}"
  if Array.all reservedWords (λ s => s != n) then
    return n
  else
    fail "reserved"

def blanksP : P Unit := do
  discard $ many' (satisfy fun c => [' ', '\n', '\t'].contains c)

-- Some aliases
def wsc := blanksP
def ws := blanksP

def skipChar (c : Char) : P Char := single c
def manyChars (p : P Char) : P String := do
  let cs <- many' p
  return String.mk cs

def skipString (s : String) : P Unit := do let _ <- string s
def manyStrings (p : P String) : P (Array String) := many' p

@[inline]
def getPos : P Position := do
  let s := (← get).posState
  return { sourcePos := s.sourcePos, lineOffset := s.offset }

@[inline]
def hexChar : P Nat := do
  let c ← anyChar
  if '0' ≤ c ∧ c ≤ '9' then
    pure $ c.val.toNat - '0'.val.toNat
  else if 'a' ≤ c ∧ c ≤ 'f' then
    pure $ c.val.toNat - 'a'.val.toNat
  else if 'A' ≤ c ∧ c ≤ 'F' then
    pure $ c.val.toNat - 'A'.val.toNat
  else
    fail "invalid hex character"

def escapedChar : P Char := do
  let c ← any
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

partial def stringLitteralInternal : P String := do
  if ← test <| single '"' then
    return ""
  else if ← test <| single '\\' then
      let ec ← escapedChar
      (·.push ec) <$> stringLitteralInternal
    -- as to whether c.val > 0xffff should be split up and encoded with multiple \u,
    -- the JSON standard is not definite: both directly printing the character
    -- and encoding it with multiple \u is allowed. we choose the former.
    else if let some c ← option <| satisfy (λ c => 0x0020 ≤ c.val ∧ c.val ≤ 0x10ffff) then
      (·.push c) <$> stringLitteralInternal
    else
      fail "unexpected character in string"

def stringLitteral : P String := do
  skipChar '"'
  stringLitteralInternal

@[inline]
def digitNonZero : P Nat := do
  let c ← satisfy (fun c => '1' ≤ c ∧ c ≤ '9')
  return c.val.toNat - '0'.val.toNat

@[inline]
def digitNat : P Nat := do
  let c ← satisfy (fun c => '0' ≤ c ∧ c ≤ '9')
  return c.val.toNat - '0'.val.toNat

def digitsToNat (ds : Array Nat) : Nat :=
  ds.toList.reverse.enum.foldl (λ acc (i, d) => acc + d * 10 ^ i) 0

def numP : P Number := do
  let sign : Int ←
    (do skipChar '-'; pure (-1 : Int))
      -- <|> pure 1
  let digits ← many' (digitNat)
  let whole := sign * digitsToNat digits
  let optDecimals : Option Number ← option do
    skipChar '.'
    let digits ← many' digitNat
    let exponent := digits.size
    let mantissa := whole * (10 ^ exponent : Nat) + sign * digitsToNat digits
    return { mantissa, exponent : Number }
  let numb : Number :=  optDecimals.getD <| Number.fromInt whole
  let withExp : P Number := do
    skipChar 'e' <|> skipChar 'E'
    let sign ← single '-' <|> single '+'
    let n ← digitsToNat <$> many' digitNat
    if sign = '-' then 
      return numb.shiftr n
    else
      return numb.shiftl n
  withExp <|> pure numb

def reservedWords := #[ "let", "with", "in", "if", "then", "else", "inherit", "rec" ]

def atom : P Expr :=
  numP <|> stringLitteral <|> nameP

def fvar : P Expr := ParsecM.map Expr.fvar nameP

-- def litteral : Parsec Expr := do
--   let string := ParsecM.map Expr.str stringLitteral
--   let number := do
--       let n ← num
--       ws
--       return Expr.num n
--   string <|> number

-- def operator : Parsec Operator := do
--   let dot := do
--     skipChar '.'
--     return Operator.dot
--   let merge := do
--     skipString "//"
--     return Operator.merge
--   let or := do
--     skipString "||"
--     return Operator.or_
--   let and := do
--     skipString "&&"
--     return Operator.and_
--   let append := do
--     skipString "++"
--     return Operator.append
--   let add := do
--     skipChar '+'
--     return Operator.add
--   let minus := do
--     skipChar '-'
--     return Operator.minus
--   let mul := do
--     skipChar '*'
--     return Operator.mul
--   dot <|> merge <|> or <|> and <|> append <|> add <|> minus <|> mul

mutual

-- TODO prove termination and remove partial

partial def stringInterpolation : P Expr := do
  skipChar '$'
  skipChar '{'
  let e ← expression
  skipChar '}'
  return e

partial def attrSetKey : P AttrSetKey := do
  let part := (Expr.fvar <$> name) <|> (Expr.str <$> stringLitteral) <|> stringInterpolation
  let first ← part
  let rest : Array Expr ← many do
    skipChar '.'
    part
  if rest.isEmpty then
    return AttrSetKey.name first
  else
    return AttrSetKey.expr (first :: rest.toList).toArray

-- partial def list : Parsec Expr := do
--   skipChar '['
--   let l ← many expression
--   wsc
--   skipChar ']'
--   return Expr.list l
/--
Lists must start and end with parentheses, with any number of grammar elements
in between.
This function could be implemented with `between` and `sebEndBy`, but, again,
let's try to stick to the simplest tools in this tutorial.
-/
partial def listP : P Expr := do
  discard $ single '['
  blanksP -- this is necessary for empty lists, otherwise `grammarP` would crash
          -- at ')'
  let xs : List Expr ← many' expressionP
  wsc
  discard $ single ']'
  return xs.foldr (init := .nil) fun x acc => .cons x acc


partial def attrsetInternal : P (List (AttrSetKey × Expr)) := do
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
    let v ← expressionP
    let stop ← getPos
    wsc
    skipString ";"
    ws
    let kvs ← attrsetInternal
    return kvs.append [(k, Expr.meta {start, stop} com v)]

partial def attrset : P Expr := do
  let isRec ← test <| sipString "rec"
  wsc
  skipChar '{'
  return Expr.attrset isRec (← attrsetInternal)

partial def ifStatement : P Expr := do
  skipString "if"
  ws
  let e ← expressionP
  wsc
  skipString "then"
  let t ← expressionP
  skipString "else"
  let f ← expressionP
  return Expr.ifStatement e t f

partial def withStatement : P Expr := do
  skipString "with"
  wsc
  let w ← fvar
  wsc
  skipChar ';'
  ws
  let e ← expressionP
  return Expr.withStatement w e

/--
Parse a let statement
let <assignments>+ in
-/
partial def letStatement : P Expr := do
  let assignment : P (Name × Expr) := do
    ws
    let com ← option <| manyStrings comment
    ws
    let start ← getPos
    let n ← name
    ws
    skipChar '='
    ws
    let ex ← expressionP
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
  let ex ← expressionP
  return Expr.letExpr ass ex

partial def varsp : P <| List (Name × Option Expr) × Bool := do
  wsc
  if ← test <| skipString "..." then
    return ([], true)
  let n ← name
  wsc
  let optDef ← option <| do {
    ws;
    skipChar '?';
    wsc;
    expression
  }
  ws;
  if ← test <| skipChar ',' then
    ws
    let (l, ca) ← varsp
    return ((n, optDef) :: l, ca);
  else
    return ([(n, optDef)], false);

partial def lambda : P Expr := do
  let destruct : P LBinding := do
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

partial def comment : P String := do
  ws
  skipChar '#'
  let com ← manyChars <| satisfy (λ c => c != '\n')
  ws
  return com

partial def comments : P (Option String) := option <| do
  manyStrings comment

-- partial def wsc : Parsec Unit := comments >>= (fun _ => pure ())

-- /-
-- Function application e1 e2
-- -/
-- partial def app : Parsec Expr := do
--   let e1 ← recSafeExpression
--   wsc
--   let com ← comments
--   let e2 ← expression
--   return Expr.app e1 e2

-- /-
-- Binary operation e1 `op` e2
-- -/
-- partial def operation : Parsec Expr := do
--   wsc
--   let e1 ← recSafeExpression
--   wsc
--   let com ← comments
--   wsc
--   let op ← operator
--   wsc
--   let e2 ← expression
--   return Expr.opr e1 op e2

partial def recSafeExpression : P Expr := do
  let wrappedExpression : P Expr := do
    skipChar '('
    let e ← expressionP
    skipChar ')'
    return e
  litteral
    <|> lambda
    <|> listP
    <|> attrset
    <|> ifStatement
    <|> letStatement
    <|> fvar
    <|> stringInterpolation
    <|> wrappedExpression

partial def expressionP : P Expr := do
  let com ← comments
  ws
  let start ← getPos
  let expr ←
    -- operation
    -- <|> app
    recSafeExpression
  let stop ← getPos
  return Expr.meta {start, stop} com expr

end

def fileP : P Expr := do
  ws
  let res ← expressionP
  wsc
  eof
  return res

end Parser

def parse (s : String) : Except String Nix.Expr :=
  Except.mapError toString $ Megaparsec.parse Parser.fileP s

end Nix.Expression
