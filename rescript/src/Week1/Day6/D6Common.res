let extender = target =>
  switch target {
  | x if x === "" => " "
  | y => y
  }

let groupify = fileContentLine =>
  fileContentLine
  ->Belt.Array.map(extender)
  ->SString.joinWith("", "_")
  ->Js.String2.split(" ")
  ->Belt.Array.map(x => x->Js.String2.split("_")->Belt.Array.keep(x => x !== ""))

let uniqueFromGroup = group =>
  group->SString.join("")->Js.String2.split("")->Belt.Set.String.fromArray->Belt.Set.String.toArray

let test = () => {
  let testRunner = filePath => filePath->FileReader.readFileLine->groupify->Belt.Array.length

  Test.expectInt("groupifyTest1", 1, "input/Week1/Year2020Day6.sample1.txt"->testRunner)
  Test.expectInt("groupifyTest2", 5, "input/Week1/Year2020Day6.sample2.txt"->testRunner)
}

test()
