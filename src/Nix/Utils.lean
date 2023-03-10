import Std.Data.HashMap
import Lean.Data.HashSet
open System
open Lean

namespace Std
instance (V : Type) [ToString V] [BEq V] [Hashable V] : ToString <| HashSet V where
  toString s := s.toList.toString

def HashMap.fromArray {K V : Type} [BEq K] [BEq V] [Hashable K] [Hashable V]
  (arr : Array (K × V)) : HashMap K V :=
  arr.foldl (λ map (k, v) => map.insert k v) <| mkHashMap (capacity := arr.size)

def HashSet.fromArray {V : Type} [BEq V] [Hashable V] (arr : Array V) : HashSet V :=
  arr.foldl (λ set v => set.insert v) <| mkHashSet (capacity := arr.size)

instance {V : Type} [BEq V] [Hashable V] : Hashable <| HashSet V where
  hash sv := hash sv.toList

instance {V : Type} [BEq V] [Hashable V] : BEq <| HashSet V where
  beq a b := a.toList ==  b.toList

instance {K V : Type} [BEq K] [BEq V] [Hashable K] : BEq <| HashMap K V where
  beq a b := a.toList ==  b.toList

end Std

namespace String

-- TODO
def sanitize (s : String) : String := s.replace "\"" "\\\""

def asStringLitteral (s : String) : String := s!"\"{s.sanitize}\""

def replicate (s : String) (n : Nat) : String :=
  match n with
  | 0 => s
  | Nat.succ ns => s ++ replicate s ns

end String

namespace System

def FilePath.findAllWithExt (p : FilePath) (ext : String) : IO (Array FilePath) := do
  if (← p.isDir) then
    return (← p.walkDir).filter (·.extension == some ext)
  else
    return #[]

end System
