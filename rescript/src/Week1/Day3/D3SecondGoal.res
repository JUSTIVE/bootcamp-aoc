open Belt
open D3Common

let solution = (trials: array<slant>, filePath) =>
  trials
  ->Array.map(x => x->potentialCollisionCount(grid_(filePath)))
  ->Array.map(Int.toFloat)
  ->MMath.Float.product

[slant_(1, 1), slant_(3, 1), slant_(5, 1), slant_(7, 1), slant_(1, 2)]
->solution("input/Week1/Year2020Day3.sample2.txt")
->Js.log
