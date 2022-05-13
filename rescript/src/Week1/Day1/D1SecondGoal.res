open Belt
open D1Common
open FileReader
open TTriple

let solution = filePath =>
  filePath
  ->readFileLine
  ->Array.keepMap(Int.fromString) // array<option<int>>
  ->triplize
  ->Array.keep(x => x->isTripleSumThatYear(2020))
  ->Array.getExn(0)
  ->multiplySelf

"input/Week1/Year2020Day1.sample2.txt"->solution->Js.log

let test = {
  Test.expectInt("sample1",241861950, solution("input/Week1/Year2020Day1.sample1.txt"))
}

// ppx PreProcessor
// ppx: AST -> AST

// bs-let

// let b = a >>= x >>= y

// jsoo = js_of_ocaml