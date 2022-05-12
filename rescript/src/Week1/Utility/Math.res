let sum_i = value => value->Belt.Array.reduce(0, (a, b) => a + b)
let max_i = value => value->Belt.Array.reduce(0, (a, b) => a > b ? a : b)
let ma_i = x => a => b=> x * a + b