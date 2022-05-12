open D5Common
open FileReader

let solution = filePath =>
  filePath
  ->readFileLine
  ->generateSeatID
  ->MMath.Int.max

solution("input/Week1/Year2020Day5.sample.txt")->Js.log
