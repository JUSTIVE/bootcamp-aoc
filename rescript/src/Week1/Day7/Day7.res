// open Belt

// type rec childbag = Bag(bag) | Empty
// and bag = {
//   amount: int,
//   kind: string,
// }

// let parseLine = line => {
//   let parseBagChunk = chunk => {
//     chunk->Js.log
//     let x =
//       %re("/(\d+) ([\w\s]*\.*)/")
//       ->Js.Re.exec_(chunk)
//       ->Option.map(Js.Re.captures)
//       ->Option.map(x => x->Array.keepMap(x => x->Js.Nullable.toOption))

//     Js.log(x)

//     switch x {
//     | Some([_, amount, kind]) =>
//       amount
//       ->Rscv.String.parseInt(10)
//       ->Option.flatMap(amount => Some(Bag({amount: amount, kind: kind})))
//     | _ => None
//     }
//   }

//   switch line->Js.String2.split("contain")->Array.map(Js.String.trim) {
//   | ["no other bags"] => Some(Empty)
//   | [key, value] =>
//     value
//     ->Js.String2.split(",")
//     ->Array.keepMap(parseBagChunk)
//     ->(x => (key, x)->Some)
//   | _ => None
//   }
// }

// "input/Week1/Year2020Day7.sample1.txt"->Rscv.FileReader.readFileLine->Array.map(parseLine)->Js.log
