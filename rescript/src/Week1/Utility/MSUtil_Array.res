let flatMap = (array, fn) => array->Belt.Array.map(fn)->Belt.Array.concatMany
let takeFirstWithDefault = (iterable, default) =>
  switch iterable->Belt.Array.get(0) {
  | Some(x) => x
  | None => default
  }
let has = (iterable,value) => iterable->Belt.Array.some(x => x == value)
