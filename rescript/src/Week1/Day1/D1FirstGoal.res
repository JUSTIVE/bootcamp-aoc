open Belt
open D1Common
open Rscv.FileReader
open Rscv.Tuple
open Rscv.Test

let solution = filePath =>
  filePath
  ->readFileLine
  ->Array.keepMap(Int.fromString)
  ->pairize
  ->Array.keep(x => x->isPairSumThatYear(2020))
  ->Array.map(Tuple2.multiplySelf)
  ->Rscv.Math.Int.sum

"input/Week1/Year2020Day1.sample2.txt"->solution->Js.log

let test = {
  expectInt("sample1", 514579, solution("input/Week1/Year2020Day1.sample1.txt"))
}
