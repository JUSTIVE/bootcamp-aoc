open Belt
open D5Common
open Rscv.FileReader

let findMissingSeat = seatIDs => {
  let bias = seatIDs->Rscv.Array.takeFirstWithDefault(0)
  seatIDs->Array.keepWithIndex((x, i) => i + bias !== x)->Rscv.Array.takeFirstWithDefault(0) - 1
}

let solution = filePath => filePath->readFileLine->generateSeatID->findMissingSeat

solution("input/Week1/Year2020Day5.sample2.txt")->Js.log
