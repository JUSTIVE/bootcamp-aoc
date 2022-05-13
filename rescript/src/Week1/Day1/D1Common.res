open Belt

let pairize = args =>
  args
  ->Array.map(x => {
    args->Array.map(y => (x, y))
  })
  ->Array.concatMany

let isPairSumThatYear = (pair, year) => pair->PPair.sumSelf === year

let triplize = args =>
  args
  ->Array.map(x => {
    args->Array.map(y => {
      args->Array.map(z => (x, y, z))
    })
    ->Array.concatMany
  })
  ->Array.concatMany

let isTripleSumThatYear = (triple, year) => triple->TTriple.sumSelf === year