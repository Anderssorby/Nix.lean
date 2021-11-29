
namespace Parsec
/-
Result which keeps track of the parsing state.
-/
inductive ParseResult (α : Type) where
  | success (pos : String.Iterator) (res : α)
  | error (pos : String.Iterator) (err : String)
  deriving Repr
end Parsec

/-
A function which converts an iterator to a ParseResult
-/
def Parsec (α : Type) : Type := String.Iterator → Parsec.ParseResult α

namespace Parsec

open ParseResult

instance (α : Type) : Inhabited (Parsec α) :=
  ⟨λ it => error it ""⟩

@[inline]
protected def pure (a : α) : Parsec α := λ it =>
 success it a

@[inline]
def bind {α β : Type} (f : Parsec α) (g : α → Parsec β) : Parsec β := λ it =>
  match f it with
  | success rem a => g a rem
  | error pos msg => error pos msg

instance : Monad Parsec :=
  { pure := Parsec.pure, bind }

@[inline]
def map {α β : Type} (p : Parsec α) (f : α → β) : Parsec β := do f (← p)

@[inline]
def andAppend {α : Type} [Append α] (f : Parsec α) (g : Parsec α) : Parsec α := do 
  let a ← f
  let b ← g
  return a ++ b

instance {α : Type} [Append α] : Append $ Parsec α := ⟨andAppend⟩

@[inline]
def fail (msg : String) : Parsec α := fun it =>
  error it msg

/-
Combine two parsers into one where the first takes presedence
and the second is tried if the first one fails.
-/
@[inline]
def orElse (p : Parsec α) (q : Unit → Parsec α) : Parsec α := fun it =>
  match p it with
  | success rem a => success rem a
  | error rem err =>
    if it = rem then q () it else error rem err

/-
Convert errors to none
-/
def option (p : Parsec α) : Parsec $ Option α := fun it =>
  match p it with
  | success rem a => success rem (some a)
  | error rem err => success rem (none)

def test (p : Parsec α) : Parsec Bool := fun it =>
  match p it with
  | success rem a => success rem true
  | error rem err => success it false

/-
Rewind the iterator on failure
-/
@[inline]
def attempt (p : Parsec α) : Parsec α := λ it =>
  match p it with
  | success rem res => success rem res
  | error _ err => error it err

instance : Alternative Parsec :=
{ failure := fail "", orElse }

def expectedEndOfInput := "expected end of input"

@[inline]
def eof : Parsec Unit := fun it =>
  if it.hasNext then
    error it expectedEndOfInput
  else
    success it ()

@[inline]
partial def manyCore (p : Parsec α) (acc : Array α) : Parsec $ Array α :=
  (do manyCore p (acc.push $ ←p))
  <|> pure acc

@[inline]
def many (p : Parsec α) : Parsec $ Array α := manyCore p #[]

@[inline]
def many1 (p : Parsec α) : Parsec $ Array α := do manyCore p #[←p]

@[inline]
partial def manyCharsCore (p : Parsec Char) (acc : String) : Parsec String :=
  (do manyCharsCore p (acc.push $ ←p))
  <|> pure acc

/-
Zero or more matching chars
-/
@[inline]
def manyChars (p : Parsec Char) : Parsec String := manyCharsCore p ""

/-
One or more matching chars
-/
@[inline]
def many1Chars (p : Parsec Char) : Parsec String := do manyCharsCore p (←p).toString

@[inline]
partial def manyStringsCore (p : Parsec String) (acc : String) : Parsec String :=
  (do manyStringsCore p (acc.append $ ←p))
  <|> pure acc

/-
One or more matching chars
-/
@[inline]
def many1Strings (p : Parsec String) : Parsec String := do
  manyStringsCore p (←p)

/-
Zero or more matching Strings
-/
@[inline]
def manyStrings (p : Parsec String) : Parsec String := manyStringsCore p ""

def pstring (s : String) : Parsec String := λ it =>
  let substr := it.extract (it.forward s.length)
  if substr = s then
    success (it.forward s.length) substr
  else
    error it s!"expected: {s}"

@[inline]
def skipString (s : String) : Parsec Unit := pstring s *> pure ()

def unexpectedEndOfInput := "unexpected end of input"

@[inline]
def anyChar : Parsec Char := λ it =>
  if it.hasNext then success it.next it.curr else error it unexpectedEndOfInput

@[inline]
def pchar (c : Char) : Parsec Char := attempt do
  if (←anyChar) = c then pure c else fail s!"expected: '{c}'"

@[inline]
def skipChar (c : Char) : Parsec Unit := pchar c *> pure ()

@[inline]
def digit : Parsec Char := attempt do
  let c ← anyChar
  if '0' ≤ c ∧ c ≤ '9' then c else fail s!"digit expected"

@[inline]
def hexDigit : Parsec Char := attempt do
  let c ← anyChar
  if ('0' ≤ c ∧ c ≤ '9')
   ∨ ('a' ≤ c ∧ c ≤ 'a')
   ∨ ('A' ≤ c ∧ c ≤ 'A') then c else fail s!"hex digit expected"

@[inline]
def asciiLetter : Parsec Char := attempt do
  let c ← anyChar
  if ('A' ≤ c ∧ c ≤ 'Z') ∨ ('a' ≤ c ∧ c ≤ 'z') then c else fail s!"ASCII letter expected"

@[inline]
def symbol : Parsec String := attempt do
  let c ← asciiLetter
  let rest ← manyChars (asciiLetter <|> digit)
  return s!"{c}{rest}"


@[inline]
def satisfy (p : Char → Bool) : Parsec Char := attempt do
  let c ← anyChar
  if p c then c else fail "condition not satisfied"

@[inline]
def notFollowedBy (p : Parsec α) : Parsec Unit := λ it =>
  match p it with
  | success _ _ => error it ""
  | error _ _ => success it ()

partial def skipWs (it : String.Iterator) : String.Iterator :=
  if it.hasNext then
    let c := it.curr
    if c = '\u0009' ∨ c = '\u000a' ∨ c = '\u000d' ∨ c = '\u0020' then
      skipWs it.next
    else
      it
  else
   it



@[inline]
def peek? : Parsec (Option Char) := fun it =>
  if it.hasNext then
    success it it.curr
  else
    success it none

@[inline]
def peek! : Parsec Char := do
  let some c ← peek? | fail unexpectedEndOfInput
  c

@[inline]
def skip : Parsec Unit := fun it =>
  success it.next ()

@[inline]
def ws : Parsec Unit := fun it =>
  success (skipWs it) ()
  
end Parsec
