open D5Common
open MSUtil.Test

let parseBinaryStringTest = {
  let patternBF = arbitraryBinaryMatcherPattern("B", "F")

  let runner = (oracle, input) =>
    expectInt("parseBinaryString", oracle, input->parseBinaryString(patternBF))

  runner(70, "BFFFBBF")
  runner(14, "FFFBBBF")
  runner(102, "BBFFBBF")
}
