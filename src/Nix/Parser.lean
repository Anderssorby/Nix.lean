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

partial def stringLitteral (acc : String := "") : Parsec String := do
  let c ← peek!
  if c = '"' then -- "
    skip
    acc
  else
    let c ← anyChar
    let ec ←
      if c = '\\' then
        escapedChar
      -- as to whether c.val > 0xffff should be split up and encoded with multiple \u,
      -- the JSON standard is not definite: both directly printing the character
      -- and encoding it with multiple \u is allowed. we choose the former.
      else if 0x0020 ≤ c.val ∧ c.val ≤ 0x10ffff then
        c
      else
        fail "unexpected character in string"
    stringLitteral (acc.push ec)

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
mutual
partial def list (acc : Array Expr) : Parsec (Array Expr) := do
  let hd ← expression
  let acc' := acc.push hd
  let c ← anyChar
  if c = ']' then
    acc'
  else
    list acc'

partial def attrset : Parsec (RBNode String (fun _ => Expr)) := do
  ws
  let c ← anyChar
  if c = '"' then
    let k ← stringLitteral ""
    ws
    skipString "="
    ws
    let v ← expression
    ws
    skipString ";"
    let kvs ← attrset
    kvs.insert compare k v
  else if c = '}' then
    RBNode.leaf
  else
    fail s!"unexpected character {c} in attrset"

-- takes a unit parameter so that
-- we can use the equation compiler and recursion
partial def expression : Parsec Expr := do
  let c ← peek!
  if c = '[' then
    skip; ws
    let c ← peek!
    if c = ']' then
      skip; ws
      Expr.list (Array.mkEmpty 0)
    else
      let a ← list (Array.mkEmpty 4)
      Expr.list a
  else if c = '{' then
    skip; ws
    let kvs ← attrset
    Expr.attrset kvs
  else if c = '\"' then
    skip
    let s ← stringLitteral
    ws
    Expr.str s
  else if c = 'f' then
    skipString "false"; ws
    Expr.bool false
  else if c = 't' then
    skipString "true"; ws
    Expr.bool true
  else if c = 'n' then
    skipString "null"; ws
    Expr.null
  else if c = '-' ∨ ('0' ≤ c ∧ c ≤ '9') then
    let n ← num
    ws
    Expr.num n
  else
    fail "unexpected input"
end

def any : Parsec Expr := do
  ws
  let res ← expression()
  eof
  res

end Nix.Parser

namespace Nix

def parse (s : String) : Except String Nix.Expr :=
  match Nix.Parser.any s.mkIterator with
  | Parsec.ParseResult.success _ res => Except.ok res
  | Parsec.ParseResult.error it err  => Except.error s!"offset {it.i.repr}: {err}"

end Nix