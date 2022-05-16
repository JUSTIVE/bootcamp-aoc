module Tuple2 = {
  type t<'a,'b> = ('a,'b)
  let multiplySelf = ((x, y)) => x * y

  let sumSelf = ((x, y)) => x + y
}

module Tuple3 = {
  type t<'a,'b,'c> = ('a,'b,'c)
  let multiplySelf = ((x, y, z)) => x * y * z

  let sumSelf = ((x, y, z)) => x + y + z  
}