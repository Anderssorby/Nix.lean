import Std.Data.HashMap
import Nix.Utils
namespace Nix
open Lean

structure StorePath where
  -- The hash is sufficient
  hash : String
  name : String := ""
  deriving BEq, Ord, Hashable

def StorePath.toString (sp : StorePath) : String := sp.hash

instance : ToString StorePath where
  toString := StorePath.toString

/-
A Nix derivation. Equivalent to a drv file.
-/
structure Derivation where
    outputs  : HashMap String StorePath
    -- ^ Outputs produced by this derivation where keys are output names
    inputDrvs : HashMap StorePath (HashSet String)
    -- ^ Inputs that are derivations where keys specify derivation paths and
    -- values specify which output names are used by this derivation
    inputSrcs : HashSet StorePath
    -- ^ Inputs that are sources
    system : String
    -- ^ Platform required for this derivation
    builder   : String
    -- ^ Code to build the derivation, which can be a path or a builtin function
    args      : Array String
    -- ^ Arguments passed to the executable used to build to derivation
    env       : HashMap String String
    -- ^ Environment variables provided to the executable used to build the
    -- derivation

namespace Derivation

def toString (d: Derivation) (format : Bool := false) : String :=
  s!"Derive({d.outputs.toList.map (λ (n, p) => s!"({n.asStringLitteral},{p.hash.asStringLitteral},\"\",\"\")")},"
  ++ s!"{d.inputDrvs.toList.map (λ (n, s) => s!"({n.toString.asStringLitteral},{s.toList.map String.asStringLitteral})")},"
  ++ s!"{d.inputSrcs.toList.map (λ p => s!"({p.hash.asStringLitteral},\"\",\"\")")},"
  ++ s!"{d.args.toList.map String.asStringLitteral},"
  ++ s!"{d.system.asStringLitteral},{d.builder.asStringLitteral},{d.args.toList.map String.asStringLitteral},{d.env.toList.map (λ (k, v) => s!"({k.asStringLitteral},{v.asStringLitteral})")})"

instance : ToString Derivation where
  toString := Derivation.toString

end Nix.Derivation
