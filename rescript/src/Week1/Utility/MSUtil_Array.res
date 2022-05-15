let flatMap = (array, fn) => array->Belt.Array.map(fn)->Belt.Array.concatMany
let takeFirstWithDefault = (iterable:array<'a>, default:'a) =>
  switch iterable->Belt.Array.get(0) {
  | Some(x) => x
  | None => default
  }
