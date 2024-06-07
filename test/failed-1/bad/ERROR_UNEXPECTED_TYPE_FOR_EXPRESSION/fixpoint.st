// ERROR_UNEXPECTED_TYPE_FOR_EXPRESSION

language core;

extend with #fixpoint-combinator;


fn main(n : Nat) -> Nat {
  return fix(fn(y : Nat) { return true });
}
