open Belt

// 숙제
// 1. 함수 시그니쳐를 먼저 적고 구현하기 -> 주석으로 남기기
// 2. part1 함수를 part2에서 재사용하는 방향으로 리팩토링 해보기

// let x = x + 1 in
// x (+) 1
// x: 'a
// x: int

module Test = {
  let expect:
    type a. (string, a, a) => unit =
    (name, expected, actual) => {
      switch actual {
      | x if x === expected => `${name} -- Pass ✅`
      | _ => `${name} -- Fail ❌`
      }->Js.log
    }
}
module MathUtil = {
  let sum = value => value->Array.reduce(0, (a, b) => a + b)

  let max = value => value->Array.reduce(0, (a, b) => a > b ? a : b)
}

module Domain = {
  type seat = {
    row: int,
    column: int,
  }

  type arbitraryBinaryMatcherPattern = {
    upperBound: string,
    lowerBound: string,
  }

  let applyMapper = (
    arbitraryBinaryMatcherPattern: arbitraryBinaryMatcherPattern,
    target: string,
  ): int =>
    switch target {
    | x if x === arbitraryBinaryMatcherPattern.upperBound => 1
    | __ => 0
    }

  let parseBinaryString = (target, arbitraryBinaryMatcherPattern) =>
    target
    ->Js.String2.split("")
    ->Array.reverse
    ->Array.map(applyMapper(arbitraryBinaryMatcherPattern))
    ->Array.mapWithIndex((i, x) =>
      Js.Math.pow_float(~base=2.0, ~exp=i->Int.toFloat)->Float.toInt * x
    )
    ->MathUtil.sum

  let readFileLine = (filePath): array<string> =>
    filePath->Node.Fs.readFileAsUtf8Sync->Js.String2.split("\n")

  let processSeatID = (seat: seat): int => seat.row * 8 + seat.column

  let parseBoardingPass = (boardingPass: string): seat => {
    let seat: seat = {
      row: boardingPass
      ->Js.String2.slice(~from=0, ~to_=7)
      ->parseBinaryString({upperBound: "B", lowerBound: "F"}),
      column: boardingPass
      ->Js.String2.slice(~from=7, ~to_=10)
      ->parseBinaryString({upperBound: "R", lowerBound: "L"}),
    }
    seat
  }

  let takeFirst = x =>
    switch x[0] {
    | Some(x) => x
    | None => 0
    }

  let generateSeatID = (fileContent): array<int> =>
    fileContent->Array.map(parseBoardingPass)->Array.map(processSeatID)->SortArray.Int.stableSort

  let findMissingSeat = seatIDs => {
    let bias = seatIDs->takeFirst

    seatIDs->Array.keepWithIndex((x, i) => i + bias !== x)->takeFirst - 1
  }

  //  array<string> -> array<int> -> int
  let solutionCore = fileContent => fileContent->generateSeatID->findMissingSeat

  let solution = filePath => filePath->readFileLine->solutionCore

  module DomainTest = {
    let parseBinaryStringTest = {
      Test.expect(
        "parseBinaryString",
        70,
        parseBinaryString("BFFFBBF", {upperBound: "B", lowerBound: "F"}),
      )
      Test.expect(
        "parseBinaryString",
        14,
        parseBinaryString("FFFBBBF", {upperBound: "B", lowerBound: "F"}),
      )
      Test.expect(
        "parseBinaryString",
        102,
        parseBinaryString("BBFFBBF", {upperBound: "B", lowerBound: "F"}),
      )
    }
    // let solutionCoreTest = {
    //   Test.expect(
    //     "parseBoardingPass",
    //     567,
    //     solutionCore(["BFFFBBFRRR"])
    //   );
    //   Test.expect(
    //     "parseBoardingPass",
    //     119,
    //     solutionCore(["FFFBBBFRRR"])
    //   )
    //   Test.expect(
    //     "parseBoardingPass",
    //     820,
    //     solutionCore(["BBFFBBFRLL"])
    //   )
    // }
  }
}

Domain.solution("input/Week1/Year2020Day5.sample.txt")->Js.log
