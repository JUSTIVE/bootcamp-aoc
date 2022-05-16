open Belt
open D1Common
open MSUtil.FileReader
open MSUtil.Tuple
open MSUtil.Test

let solution = filePath =>
  filePath
  ->readFileLine
  ->Array.keepMap(Int.fromString)
  ->pairize
  ->Array.keep(x => x->isPairSumThatYear(2020))
  ->Array.map(Tuple2.multiplySelf)
  ->MSUtil.Math.Int.sum

"input/Week1/Year2020Day1.sample2.txt"->solution->Js.log

let test = {
  expectInt("sample1", 514579, solution("input/Week1/Year2020Day1.sample1.txt"))
}
