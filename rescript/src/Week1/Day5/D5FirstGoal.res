open D5Common
open MSUtil.FileReader

let solution = filePath =>
  filePath
  ->readFileLine
  ->generateSeatID
  ->MSUtil.Math.Int.max

solution("input/Week1/Year2020Day5.sample2.txt")->Js.log
