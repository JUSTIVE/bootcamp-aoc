open Belt

let pairize = args =>
  args->AArray.flatmap(x => {
    args->Array.map(y => (x, y))
  })

let isPairSumThatYear = (pair, year) => pair->PPair.sumSelf === year

let triplize = args =>
  args->AArray.flatmap(x => {
    args->AArray.flatmap(y => {
      args->Array.map(z => (x, y, z))
    })
  })
  
let isTripleSumThatYear = (triple, year) => triple->TTriple.sumSelf === year
