// open Belt

// type rule = {
//   least: int,
//   most: int,
//   kind: string,
// }
// type parsedLine = {
//   rule: rule,
//   value: string,
// }

// let parse = line => {
//   let [prefix, value] = line->Js.String.split(": ")
//   let [prefix, kind] = prefix->Js.String.split(" ")
//   let [least, most] = prefix->Js.String.split("-")
//   {
//     rule: {
//       least: least->Int.fromString->Option.getExn,
//       most: most->Int.fromString->Option.getExn,
//       kind: kind,
//     },
//     value: value,
//   }
// }

// let test = parsedline =>{
//   let {rule, value} = parsedline
//   let {least, most, kind} = rule
//   let count = value->Js.String.split("")->List.length
//   count->MMath.Int.inRange(least,most)
  
// }

// 모듈 펑터
// Belt.Map 모듈 펑터 여서 사용하는 법 