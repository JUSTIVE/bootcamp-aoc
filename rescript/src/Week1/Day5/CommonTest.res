open Common

let parseBinaryStringTest = {
  let patternBF = arbitraryBinaryMatcherPattern("B", "F")
  let runner = (oracle, input) =>
    Test.expect("parseBinaryString", oracle, input->parseBinaryString(patternBF))

  runner(70, "BFFFBBF")
  runner(14, "FFFBBBF")
  runner(102, "BBFFBBF")
}
