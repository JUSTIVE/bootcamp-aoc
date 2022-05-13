open Belt
open D1Common
open FileReader
open PPair

let solution = filePath =>
  filePath
  ->readFileLine
  ->Array.keepMap(Int.fromString)
  ->pairize
  ->Array.keep(x => x->isPairSumThatYear(2020))
  ->Array.map(multiplySelf)
  ->MMath.Int.sum


"input/Week1/Year2020Day1.sample2.txt"->solution->Js.log

let test = {
  Test.expectInt("sample1",514579, solution("input/Week1/Year2020Day1.sample1.txt"))
}