open Belt

type rule = {
  least: int,
  most: int,
  kind: string,
}
type parsedLine = {
  rule: rule,
  value: string,
}

let safeSplit = (string, delim) =>
  switch string->Js.String2.split(delim) {
  | [a, b] => (a, b)
  | __ => ("", "")
  }

let parse = line => {
  let (prefix, value) = line->safeSplit(": ")
  let (prefix, kind) = prefix->safeSplit(" ")
  let (least, most) = prefix->safeSplit("-")

  {
    rule: {
      least: least->Int.fromString->Option.getExn,
      most: most->Int.fromString->Option.getExn,
      kind: kind,
    },
    value: value,
  }
}

let process1 = ({rule, value}) =>
  value->SString.count(rule.kind)->MMath.Int.inRange(rule.least, rule.most)

let process2 = ({rule, value}) =>
  BBool.xor(
    value->Js.String2.charAt(rule.least-1) === rule.kind,
    value->Js.String2.charAt(rule.most-1) === rule.kind,
  )

let goal1 = filePath =>
  filePath->FileReader.readFileLine->Array.map(parse)->Array.keep(process1)->Array.length->Js.log

let goal2 = filePath =>
  filePath
  ->FileReader.readFileLine
  ->Array.map(parse)
  ->Array.keep(process2)
  ->Array.length
  ->Js.log

// "input/Week1/Year2020Day2.sample2.txt"->goal1

"input/Week1/Year2020Day2.sample2.txt"->goal2

// let test = parsedline => {
//   let {rule, value} = parsedline
//   let {least, most, kind} = rule
//   let count = value->Js.String.split("")->List.length
//   count->MMath.Int.inRange(least, most)
// }

// 모듈 펑터
// Belt.Map 모듈 펑터 여서 사용하는 법 