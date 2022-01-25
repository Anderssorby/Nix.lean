import Std
namespace Nix
open Std

structure StorePath where
  -- The hash is sufficient
  hash : String
  name : String := ""
  deriving BEq, Ord, Hashable

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
    -- deriving BEq, Ord

namespace Derivation

end Nix.Derivation
