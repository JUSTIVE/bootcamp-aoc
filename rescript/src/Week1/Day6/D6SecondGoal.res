open Belt
open D6Common

let gatherYes = group =>
  [
    'a',
    'b',
    'c',
    'd',
    'e',
    'f',
    'g',
    'h',
    'i',
    'j',
    'k',
    'l',
    'm',
    'n',
    'o',
    'p',
    'q',
    'r',
    's',
    't',
    'u',
    'v',
    'w',
    'x',
    'y',
    'z',
  ]
  ->Array.keep(alphabet => group->Array.every(x => x->String.contains(alphabet)))
  ->Array.length

let solution = filePath =>
  filePath
  ->FileReader.readFileLine
  ->groupify
  ->Array.map(gatherYes)
  ->MMath.Int.sum

"input/Week1/Year2020Day6.sample3.txt"->solution->Js.log

let test = {
  Test.expectInt("sample1", 3, solution("input/Week1/Year2020Day6.sample1.txt"))
  Test.expectInt("sample2", 6, solution("input/Week1/Year2020Day6.sample2.txt"))
  Test.expectInt("gatherYes1", 3,gatherYes(["abc"]))
  Test.expectInt("gatherYes2", 2,gatherYes(["abc","ab"]))
}
