open D5Common

let parseBinaryStringTest = {
  let patternBF = arbitraryBinaryMatcherPattern("B", "F")

  let runner = (oracle, input) =>
    Test.expectInt("parseBinaryString", oracle, input->parseBinaryString(patternBF))

  runner(70, "BFFFBBF")
  runner(14, "FFFBBBF")
  runner(102, "BBFFBBF")
}
