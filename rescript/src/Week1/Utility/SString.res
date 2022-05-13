let joinWith = (args, init, delim) => Belt.Array.reduce(args, init, (a, b) => a ++ delim ++ b)

let join = (args, init) => joinWith(args, init, "")

let count = (value, token) =>
  value->Js.String2.split("")->Belt.Array.keep(x => x === token)->Belt.Array.length
