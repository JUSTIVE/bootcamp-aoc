open D5Common
open FileReader
open Math

let solution = filePath =>
  filePath
  ->readFileLine
  ->generateSeatID
  ->max_i

solution("input/Week1/Year2020Day5.sample.txt")->Js.log
