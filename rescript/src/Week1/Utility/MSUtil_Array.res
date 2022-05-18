let flatMap = (array, fn) => array->Belt.Array.map(fn)->Belt.Array.concatMany
let takeFirstWithDefault = (iterable, default) =>
  switch iterable->Belt.Array.get(0) {
  | Some(x) => x
  | None => default
  }

let everyO = (iterable)=>
  switch iterable -> Belt.Array.every(Belt.Option.isSome){
  | true => iterable->Belt.Array.map(Belt.Option.getExn)->Some
  | false => None
  }

let take = (iterable, indicies) =>
  indicies
  ->Belt.Array.map(index=>iterable->Belt.Array.get(index))
  ->everyO
let has = (iterable,value) => iterable->Belt.Array.some(x => x == value)