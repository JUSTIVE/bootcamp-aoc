open Belt
open D6Common

let solution = filePath =>
  filePath
  ->FileReader.readAllFile
  ->parseFile
  ->Array.map(union)
  ->Array.map(Belt.Set.String.size)
  ->MMath.Int.sum

"input/Week1/Year2020Day6.sample1.txt"->solution->Js.log

let test = {
  Test.expectInt("sample1", 6, solution("input/Week1/Year2020Day6.sample1.txt"))
  Test.expectInt("sample2", 11, solution("input/Week1/Year2020Day6.sample2.txt"))
}
