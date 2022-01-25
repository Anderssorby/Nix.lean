import Std
open Std

instance (V : Type) [ToString V] [BEq V] [Hashable V] : ToString <| HashSet V where
  toString s := s.toList.toString

def HashMap.fromArray {K V : Type} [BEq K] [BEq V] [Hashable K] [Hashable V]
  (arr : Array (K × V)) : HashMap K V :=
  arr.foldl (λ map (k, v) => map.insert k v) <| mkHashMap (capacity := arr.size)

def HashSet.fromArray {V : Type} [BEq V] [Hashable V] (arr : Array V) : HashSet V :=
  arr.foldl (λ set v => set.insert v) <| mkHashSet (nbuckets := arr.size)

instance {V : Type} [BEq V] [Hashable V] : Hashable <| HashSet V where
  hash sv := hash sv.toList

instance {V : Type} [BEq V] [Hashable V] : BEq <| HashSet V where
  beq a b := a.toList ==  b.toList
