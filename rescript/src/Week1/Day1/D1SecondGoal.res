open Belt
open D1Common
open FileReader
open Triple

let solution = filePath =>
  filePath
  ->readFileLine
  ->Array.map(Int.fromString)
  ->Array.keep(Option.isSome)
  ->Array.map(Option.getExn)
  ->triplize
  ->Array.keep(x => x->isTripleSumThatYear(2020))
  ->Array.getExn(0)
  ->multiplySelf

"input/Week1/Year2020Day1.sample2.txt"->solution->Js.log

let test = {
  Test.expectInt("sample1",514579, solution("input/Week1/Year2020Day1.sample1.txt"))
}