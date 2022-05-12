let extender = target =>
  switch target {
  | x if x === "" => " "
  | y => y
  }

let groupify = fileContentLine =>
  fileContentLine
  ->Belt.Array.map(extender)
  ->Belt.Array.reduce("", (a, b) => a ++ b)
  ->Js.String2.split(" ")

let uniqueFromGroup = group =>
  group->Js.String2.split("")->Belt.Set.String.fromArray->Belt.Set.String.toArray

let test = () => {
  let testRunner = filePath => filePath->FileReader.readFileLine->groupify->Belt.Array.length

  Test.expectInt("groupifyTest1", 1, "input/Week1/Year2020Day6.sample.txt"->testRunner)
  Test.expectInt("groupifyTest2", 5, "input/Week1/Year2020Day6.sample2.txt"->testRunner)
}

test()
