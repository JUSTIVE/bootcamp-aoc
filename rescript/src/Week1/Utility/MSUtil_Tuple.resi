module Tuple2: {
  type t<'a,'b>
  let multiplySelf: ((int, int)) => int

  let sumSelf: ((int, int)) => int
}

module Tuple3: {
  type t<'a,'b,'c>
  let multiplySelf: ((int, int, int)) => int

  let sumSelf: ((int, int, int)) => int
}