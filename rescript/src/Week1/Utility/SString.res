let joinWith = (args, init, delim) => Belt.Array.reduce(args, init, (a, b) => a ++ delim ++ b)

let join = (args, init) => joinWith(args, init, "")
