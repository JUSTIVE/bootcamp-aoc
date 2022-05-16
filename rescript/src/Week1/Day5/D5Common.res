open Belt

type seat = {
  row: int,
  column: int,
}

type arbitraryBinaryPattern = {
  upperBound: string,
  lowerBound: string,
}

let seat = (row, column) => {
  {
    row: row,
    column: column,
  }
}

let arbitraryBinaryPattern = (upperBound, lowerBound) => {
  {
    upperBound: upperBound,
    lowerBound: lowerBound,
  }
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
  ->MSUtil.String.join("")

let processSeatID = seat => MSUtil.Math.Int.ma(8, seat.row, seat.column)

let parseBoardingPass = boardingPass => {
  seat(
    boardingPass
    ->Js.String2.slice(~from=0, ~to_=7)
    ->stringToBinary({upperBound: "B", lowerBound: "F"})
    ->MSUtil.String.parseInt(2),
    boardingPass
    ->Js.String2.slice(~from=7, ~to_=10)
    ->stringToBinary({upperBound: "R", lowerBound: "L"})
    ->MSUtil.String.parseInt(2),
  )
}

let findMissingSeat = seatIDs => {
  let bias = seatIDs->MSUtil.Array.takeFirstWithDefault(0)
  seatIDs->Array.keepWithIndex((x, i) => i + bias != x)->MSUtil.Array.takeFirstWithDefault(0) - 1
}

let generateSeatID = fileContent =>
  fileContent->Array.map(parseBoardingPass)->Array.map(processSeatID)->SortArray.Int.stableSort
