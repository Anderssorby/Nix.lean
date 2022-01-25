import Std
import Nix.Utils
import Nix.Derivation.Types
import Nix.Parsec

open Parsec
open Std
namespace Nix.Derivation

namespace Parser

def StorePath.parse (p : String) : StorePath :=
  -- TODO parse storePath
  { hash := p : StorePath }

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

def stringLitteral : Parsec String := do
  skipChar '"'
  let internals : Parsec Char := do 
    let c ← anyChar
    if c = '\\' then
      escapedChar
    -- Permitted range
    else if 0x0020 ≤ c.val ∧ c.val ≤ 0x10ffff then
      c
    else
      fail "unexpected character in string"
  let s ← manyChars internals
  skipChar '"'
  return s

def listOfInternal {A : Type} (p : Parsec A) : Parsec <| Array A := do
  let elem := do
    ws
    let a ← p
    ws
    skipChar ','
    pure a
  let elements ← many elem
  ws
  if let some last ← option p then
    return (elements.push last)
  else
    return elements

def HashMap.fromArray {K V : Type} [BEq K] [BEq V] [Hashable K] [Hashable V]
  (arr : Array (K × V)) : HashMap K V :=
  arr.foldl (λ map (k, v) => map.insert k v) <| mkHashMap (capacity := arr.size)

def HashSet.fromArray {V : Type} [BEq V] [Hashable V] (arr : Array V) : HashSet V :=
  arr.foldl (λ set v => set.insert v) <| mkHashSet (nbuckets := arr.size)

instance {V : Type} [BEq V] [Hashable V] : Hashable <| HashSet V where
  hash sv := hash sv.toList

instance {V : Type} [BEq V] [Hashable V] : BEq <| HashSet V where
  beq a b := a.toList ==  b.toList

def listOf {A : Type} (p : Parsec A) (startBracket : Char := '[') (endBracket : Char := ']') : Parsec <| Array A := do
  skipChar startBracket
  ws
  let elements ← listOfInternal p
  ws
  skipChar endBracket
  return elements

def derivation : Parsec Derivation := do
  let outputP : Parsec (String × StorePath) := do
    skipChar '('
    let name ← stringLitteral
    ws; skipChar ','; ws
    let storePath ← StorePath.parse <$> stringLitteral
    ws; skipChar ','; ws
    let _hashAlgo ← stringLitteral
    ws; skipChar ','; ws
    let _hash ← stringLitteral
    skipChar ')'
    return (name, storePath)
  let inputDrvP : Parsec (StorePath × HashSet String) := do
    skipChar '('
    let storePath ← StorePath.parse <$> stringLitteral
    ws; skipChar ','; ws
    let outs ← HashSet.fromArray <$> listOf stringLitteral
    skipChar ')'
    return (storePath, outs)
  let envP : Parsec (String × String) := do
    skipChar '('
    let name ← stringLitteral
    ws; skipChar ','; ws
    let value ← stringLitteral
    skipChar ')'
    return (name, value)
  skipString "Derive"
  ws; skipChar '('; ws
  let outputs ← HashMap.fromArray <$> listOf outputP
  ws; skipChar ','; ws
  let inputDrvs ← HashMap.fromArray <$> listOf inputDrvP
  ws; skipChar ','; ws
  let inputSrcs ← HashSet.fromArray <$> (Array.map StorePath.parse) <$> listOf stringLitteral
  ws; skipChar ','; ws
  let system ← stringLitteral
  ws; skipChar ','; ws
  let builder ← stringLitteral
  ws; skipChar ','; ws
  let args ← listOf stringLitteral
  ws; skipChar ','; ws
  let env ← HashMap.fromArray <$> listOf envP
  skipChar ')'
  return { outputs, inputDrvs, inputSrcs, system, builder, args, env : Derivation }
  
  
end Parser

def parse (s : String) : Except String Nix.Derivation :=
  match Parser.derivation { it := s.mkIterator : Parsec.Pos } with
  | Parsec.ParseResult.success _ res => Except.ok res
  | Parsec.ParseResult.error pos err  => Except.error s!"{err} ({pos.line}:{pos.lineOffset})"

end Nix.Derivation
