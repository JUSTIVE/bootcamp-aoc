open Belt
open D6Common

let solution = filePath =>
  filePath
  ->FileReader.readFileLine
  ->groupify
  ->Array.map(uniqueFromGroup)
  ->Array.map(Array.length)
  ->MMath.Int.sum

"input/Week1/Year2020Day6.sample3.txt"->solution->Js.log

let test = {
  Test.expectInt("sample1", 6, solution("input/Week1/Year2020Day6.sample1.txt"))
  Test.expectInt("sample2", 11, solution("input/Week1/Year2020Day6.sample2.txt"))
}
