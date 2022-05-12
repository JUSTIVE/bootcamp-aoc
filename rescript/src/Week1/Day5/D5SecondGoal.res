open Belt
open D5Common
open FileReader

let findMissingSeat = seatIDs => {
  let bias = seatIDs->takeFirst(0)
  seatIDs->Array.keepWithIndex((x, i) => i + bias !== x)->takeFirst(0) - 1
}

let solution = filePath => filePath->readFileLine->generateSeatID->findMissingSeat

solution("input/Week1/Year2020Day5.sample.txt")->Js.log
