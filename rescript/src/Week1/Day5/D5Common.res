open Belt

type seat = {
  row: int,
  column: int,
}

type arbitraryBinaryMatcherPattern = {
  upperBound: string,
  lowerBound: string,
}

let seat = (row, column) => {
  {
    row: row,
    column: column,
  }
}

let arbitraryBinaryMatcherPattern = (upperBound, lowerBound) => {
  {
    upperBound: upperBound,
    lowerBound: lowerBound,
  }
}

let applyMapper = (arbitraryBinaryMatcherPattern, target) =>
  switch target {
  | x if x === arbitraryBinaryMatcherPattern.upperBound => 1
  | __ => 0
  }

let parseBinaryString = (target, arbitraryBinaryMatcherPattern) =>
  target
  ->Js.String2.split("")
  ->Array.reverse
  ->Array.map(applyMapper(arbitraryBinaryMatcherPattern))
  ->Array.mapWithIndex((i, x) => Js.Math.pow_float(~base=2.0, ~exp=i->Int.toFloat)->Float.toInt * x)
  ->MSUtil.Math.Int.sum

let processSeatID = seat => MSUtil.Math.Int.ma(8, seat.row, seat.column)

let parseBoardingPass = boardingPass => {
  seat(
    boardingPass
    ->Js.String2.slice(~from=0, ~to_=7)
    ->parseBinaryString({upperBound: "B", lowerBound: "F"}),
    boardingPass
    ->Js.String2.slice(~from=7, ~to_=10)
    ->parseBinaryString({upperBound: "R", lowerBound: "L"}),
  )
}

let takeFirst = (iterable, fallback) =>
  switch iterable[0] {
  | Some(x) => x
  | None => fallback
  }

let findMissingSeat = seatIDs => {
  let bias = seatIDs->takeFirst(0)
  seatIDs->Array.keepWithIndex((x, i) => i + bias !== x)->takeFirst(0) - 1
}

let generateSeatID = fileContent =>
  fileContent->Array.map(parseBoardingPass)->Array.map(processSeatID)->SortArray.Int.stableSort
