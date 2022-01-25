import Nix

open Nix
open System

def main (args : List String) : IO UInt32 := do
  try
    let file := FilePath.mk <| args.get! 0 
    let src ← IO.FS.readFile file
    if args.contains "--drv" || args.contains "-d" then
      match Derivation.parse src with
      | Except.ok drv => IO.println <| ToString.toString drv
      | Except.error e => IO.eprintln s!"Error: {e}"
    else
      match Expression.parse src with
      | Except.ok expr => IO.println <| ToString.toString expr
      | Except.error e => IO.eprintln s!"Error: {e}"
    pure 0
  catch e =>
    IO.eprintln <| "Error: " ++ toString e -- avoid "uncaught exception: ..."
    pure 1

