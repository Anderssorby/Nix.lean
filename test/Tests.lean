import Nix

open Nix
open System

def main (args : List String) : IO UInt32 := do
  try
    let files := List.map FilePath.mk [ "test/basic_array.nix", "test/attrset.nix", "test/let.nix" ]
    let srcs ← files.mapM (λ fp => do (fp, ←IO.FS.readFile fp))
    for (fp, src) in srcs do
      println! "Parsing {fp}"
      match Expression.parse src with
      | Except.ok expr => IO.println <| ToString.toString expr
      | Except.error e => IO.eprintln s!"Error: {e}"
    pure 0
  catch e =>
    IO.eprintln <| "error: " ++ toString e -- avoid "uncaught exception: ..."
    pure 1

