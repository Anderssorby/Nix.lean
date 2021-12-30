import Nix

open Nix
open System

def main (args : List String) : IO UInt32 := do
  try
    let file := FilePath.mk <| args.get! 0 
    let src ← IO.FS.readFile file
    (match parse src with
    | Except.ok expr => IO.println <| ToString.toString expr
    | Except.error e => println! "Error: {e}"
    )
    pure 0
  catch e =>
    IO.eprintln <| "error: " ++ toString e -- avoid "uncaught exception: ..."
    pure 1

