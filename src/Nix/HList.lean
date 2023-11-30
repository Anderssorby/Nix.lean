namespace Nix

/--
Hetrogenous list of types paramtereized by ɑ
-/
inductive HList {ɑ : Type v} (β : ɑ → Type u) : List ɑ → Type (max u v)
  | nil : HList β []
  | cons : β i → HList β is → HList β (i::is)

infix:67 " :: " => HList.cons

notation "[" "]" => HList.nil


