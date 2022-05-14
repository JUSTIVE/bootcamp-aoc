open Belt
open D6Common
open MSUtil.FileReader
open MSUtil.Test

let solution = filePath =>
  filePath
  ->readAllFile
  ->parseFile
  ->Array.map(union)
  ->Array.map(Belt.Set.String.size)
  ->MSUtil.Math.Int.sum

"input/Week1/Year2020Day6.sample1.txt"->solution->Js.log

let test = {
  expectInt("sample1", 6, solution("input/Week1/Year2020Day6.sample1.txt"))
  expectInt("sample2", 11, solution("input/Week1/Year2020Day6.sample2.txt"))
}
