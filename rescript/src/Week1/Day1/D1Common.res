open Belt
open MSUtil.Array
open MSUtil.Tuple

let pairize = args =>
  args->flatMap(x => {
    args->Array.map(y => (x, y))
  })

let isPairSumThatYear = (pair, year) => pair->Tuple2.sumSelf === year

let triplize = args =>
  args->flatMap(x => {
    args->flatMap(y => {
      args->Array.map(z => (x, y, z))
    })
  })

let isTripleSumThatYear = (triple, year) => triple->Tuple3.sumSelf === year
