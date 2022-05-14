open Belt
open MSUtil.FileReader
open MSUtil.Bool
open MSUtil.String

type rule = {
  least: int,
  most: int,
  kind: string,
}

type parsedLine = {
  rule: rule,
  value: string,
}

let parse = line =>
  // 리스크립트 Regex 리팩토링 해보기
  %re("/(\d)-(\d) (\w): (\w+)/")
  ->Js.Re.exec_(line)
  ->Option.flatMap(result =>
    switch result->Js.Re.captures->Array.keepMap(Js.toOption) {
    | [_, least, most, kind, value] =>
      Some({
        rule: {
          least: least->Int.fromString->Option.getExn,
          most: most->Int.fromString->Option.getExn,
          kind: kind,
        },
        value: value,
      })
    | _ => None
    }
  )

let validate1 = ({rule, value}) =>
  value->count(rule.kind)->MSUtil.Math.Int.isInRange(rule.least, rule.most)

let validate2 = ({rule, value}) =>
  xor(
    value->Js.String2.charAt(rule.least - 1) === rule.kind,
    value->Js.String2.charAt(rule.most - 1) === rule.kind,
  )

let goal1 = filePath =>
  filePath
  ->readFileLine
  ->Array.map(parse)
  ->Array.keep(Option.isSome)
  ->Array.keep(x => x->Option.mapWithDefault(false, validate1))
  ->Array.length
  ->Js.log

let goal2 = filePath =>
  filePath
  ->readFileLine
  ->Array.map(parse)
  ->Array.keep(x => x->Option.mapWithDefault(false, validate2))
  ->Array.length
  ->Js.log

"input/Week1/Year2020Day2.sample1.txt"->goal1

"input/Week1/Year2020Day2.sample1.txt"->goal2

// 모듈 펑터
// Belt.Map 모듈 펑터 여서 사용하는 법 
