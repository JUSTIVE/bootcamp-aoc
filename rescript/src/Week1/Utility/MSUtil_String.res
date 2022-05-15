let joinWith = (args, init, delim) => Belt.Array.reduce(args, init, (a, b) => a ++ delim ++ b)

let join = (args, init) => joinWith(args, init, "")

let count = (value, token) =>
  value->Js.String2.split("")->Belt.Array.keep(x => x === token)->Belt.Array.length

let parseInt = (value, radix) =>
  value
  ->Js.String2.split("")
  ->Belt.Array.map(c => c->Js.String2.charCodeAt(0)->Belt.Float.toInt - 48)
  ->Belt.Array.reverse
  ->Belt.Array.mapWithIndex((i, x) =>
    Js.Math.pow_float(~base=radix->Belt.Int.toFloat, ~exp=i->Belt.Int.toFloat)->Belt.Float.toInt * x
  )
  ->MSUtil_Math.Int.sum
