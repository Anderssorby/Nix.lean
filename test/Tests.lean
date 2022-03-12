import Nix
import Nix.Utils
import Tests.Expression
import Tests.Derivation

open Nix System Tests

def main (args : List String) : IO UInt32 := do
  try
    Expression.tests
    Derivation.tests
    pure 0
  catch e =>
    IO.eprintln <| "error: " ++ toString e
    pure 1
