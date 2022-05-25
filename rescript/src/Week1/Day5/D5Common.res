open Belt

type seat = {
  row: int,
  column: int,
}

type arbitraryBinaryPattern = {
  upperBound: string,
  lowerBound: string,
}

let stringToBinary = (value, arbitraryBinaryPattern) =>
  value
  ->Js.String2.split("")
  ->Array.map(value =>
    switch value {
    | x if x == arbitraryBinaryPattern.upperBound => "1"
    | __ => "0"
    }
  )
  ->Rscv.String.join("")

let processSeatID = seat => Rscv.Math.Int.ma(8, seat.row, seat.column)

let parseBoardingPass = boardingPass => {
  switch (
    boardingPass
    ->Js.String2.slice(~from=0, ~to_=7)
    ->stringToBinary({upperBound: "B", lowerBound: "F"})
    ->Rscv.String.parseInt(2),
    boardingPass
    ->Js.String2.slice(~from=7, ~to_=10)
    ->stringToBinary({upperBound: "R", lowerBound: "L"})
    ->Rscv.String.parseInt(2),
  ) {
  | (Some(row), Some(column)) => Some({row: row, column: column})
  | _ => None
  }
}

let findMissingSeat = seatIDs => {
  let bias = seatIDs->Rscv.Array.takeFirstWithDefault(0)
  seatIDs->Array.keepWithIndex((x, i) => i + bias != x)->Rscv.Array.takeFirstWithDefault(0) - 1
}

let generateSeatID = fileContent =>
  fileContent
  ->Array.map(parseBoardingPass)
  ->Array.map(x => x->Option.map(processSeatID))
  ->Array.keep(Option.isSome)
  ->Array.map(x => x->Option.getExn)
  ->SortArray.Int.stableSort
