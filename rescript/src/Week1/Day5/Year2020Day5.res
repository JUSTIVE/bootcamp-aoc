open Belt

module Test = {

  type expect<'a> = (string,'a,'a)=>unit
  let expect:expect<'a> = (name, expected:'a,actual:'a) => {
    switch actual {
    | x if x === expected=> `${name} -- Pass ✅`
    | __ => `${name} -- Fail ❌`
    }
    ->Js.log
  };
}
module MathUtil = {
  let sum = (value: array<int>): int => 
    value
    ->Array.reduce(0, (a: int, b: int) => a + b)

  let max = (value): int => 
    value->
    Array.reduce(0, (a, b) => a > b ? a : b)
}

module Domain = {
  type seatInfo = {
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

  let parseBinaryString = (
    target: string,
    arbitraryBinaryMatcherPattern: arbitraryBinaryMatcherPattern,
  ): int =>
    target
    ->Js_string2.split("")
    ->Js.Array.reverseInPlace
    ->Array.map(applyMapper(arbitraryBinaryMatcherPattern))
    ->Array.mapWithIndex((i, x) =>
      Js.Math.pow_float(~base=2.0, ~exp=i->Int.toFloat)
      ->Float.toInt * x
    )
    ->MathUtil.sum

  let readFileLine = (filePath): array<string> =>
    filePath
    ->Node.Fs.readFileAsUtf8Sync
    ->Js_string2.split("\n")

  let processSeatID = (seatInfo: seatInfo): int => 
    seatInfo.row * 8 + seatInfo.column

  let parseBoardingPass = (boardingPass:string): seatInfo => {
    let seatInfo:seatInfo = {
      row: (
          boardingPass
          ->Js_string.slice(~from=0, ~to_=7)
          ->parseBinaryString({upperBound: "B", lowerBound: "F"})
      ),
      column: (
          boardingPass
          ->Js_string.slice(~from=7, ~to_=10)
          ->parseBinaryString({upperBound: "R", lowerBound: "L"})
      )
    }
    seatInfo
  }

  let solutionCore = (fileContent)=>
    fileContent
    ->Array.map(parseBoardingPass)
    ->Array.map(processSeatID)
    ->MathUtil.max

  let solution = (filePath: string) =>
    readFileLine(filePath)
    ->solutionCore

  module DomainTest = {
    let parseBinaryStringTest = {
      Test.expect(
        "parseBinaryString",
        70,
        parseBinaryString("BFFFBBF", {upperBound: "B", lowerBound: "F"})
      );
      Test.expect(
        "parseBinaryString",
        14,
        parseBinaryString("FFFBBBF", {upperBound: "B", lowerBound: "F"})
      );
      Test.expect(
        "parseBinaryString",
        102,
        parseBinaryString("BBFFBBF", {upperBound: "B", lowerBound: "F"})
      );
    }
    let solutionCoreTest = {
      Test.expect(
        "parseBoardingPass",
        567,
        solutionCore(["BFFFBBFRRR"])
      );
      Test.expect(
        "parseBoardingPass",
        119,
        solutionCore(["FFFBBBFRRR"])
      )
      Test.expect(
        "parseBoardingPass",
        820,
        solutionCore(["BBFFBBFRLL"])
      )
    }
  }
}

Domain.solution("input/Week1/Year2020Day5.sample.txt")
->Js.log
