open D5Common
open Rscv.FileReader

let solution = filePath =>
  filePath
  ->readFileLine
  ->generateSeatID
  ->Rscv.Math.Int.max

solution("input/Week1/Year2020Day5.sample2.txt")->Js.log
