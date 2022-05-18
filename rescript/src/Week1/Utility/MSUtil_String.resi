let join: (array<string>, string) => string
let joinWith: (array<string>, string, string) => string
let count: (string, string) => int
let parseInt: (string, int) => option<int>

module Hex: {
  type t

  let make: string => option<t>
}
