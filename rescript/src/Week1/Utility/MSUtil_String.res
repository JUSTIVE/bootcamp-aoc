let joinWith = (args, init, delim) => Belt.Array.reduce(args, init, (a, b) => a ++ delim ++ b)

let join = (args, init) => joinWith(args, init, "")

let count = (value, token) =>
  value->Js.String2.split("")->Belt.Array.keep(x => x == token)->Belt.Array.length

let parseInt = (value, radix) => {
  let parseIntSingleString = value =>
    switch value->Js.String2.charCodeAt(0)->Belt.Float.toInt - 48 {
    | x if x >= 0 && x <= 9 => Some(x)
    | _ => None
    }

  switch value->Js.String2.split("")->Belt.Array.map(parseIntSingleString) {
  | x if Belt.Array.every(x, Belt.Option.isSome) =>
    x
    ->Belt.Array.reverse
    ->Belt.Array.map(x => x->Belt.Option.getExn)
    ->Belt.Array.mapWithIndex((i, x) =>
      Js.Math.pow_float(~base=radix->Belt.Int.toFloat, ~exp=i->Belt.Int.toFloat)->Belt.Float.toInt *
        x
    )
    ->MSUtil_Math.Int.sum
    ->Some
  | _ => None
  }
}

module Hex = {
  type t = string
  let make = string =>
    %re("/(#[0-9a-f]+)/")
    ->Js.Re.exec_(string)
    ->Belt.Option.map(x => x->Js.Re.captures)
    ->Belt.Option.flatMap(x => x->Belt.Array.get(1))
    ->Belt.Option.flatMap(Js.Nullable.toOption)
}
