import Nix
import Nix.Utils

open Nix
open System

-- #eval Expression.Parser.letStatement.parse "let\n a = \"\";\n in a"

def main (args : List String) : IO UInt32 := do
  try
    let files ← (FilePath.mk "test/nix").findAllWithExt "nix"
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

