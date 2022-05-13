open Belt
open D6Common


let solution = filePath =>
  filePath
  ->FileReader.readAllFile
  ->parseFile
  ->Array.map(D6Common.intersection)
  ->Array.map(Belt.Set.String.size)
  ->MMath.Int.sum

"input/Week1/Year2020Day6.sample2.txt"->solution->Js.log

let test = {
  Test.expectInt("sample1", 3, solution("input/Week1/Year2020Day6.sample1.txt"))
  Test.expectInt("sample2", 6, solution("input/Week1/Year2020Day6.sample2.txt"))
}
