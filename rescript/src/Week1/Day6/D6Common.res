open Belt

type group = array<Set.String.t>

let parseChunk = chunk =>
  chunk->Js.String2.split("\n")->Array.map(line => line->Js.String2.split("")->Set.String.fromArray)

let parseFile = fileContent => fileContent->Js.String2.split("\n\n")->Array.map(parseChunk)

let union = (group: group) => group->Array.reduce(Set.String.empty, Set.String.union)

let intersection = (group: group) => group->Array.reduce(group->union, Set.String.intersect)

// 1. 파싱을 "\n\n" split 해서 개선해본다.
// 2. part1: parse -> union -> length, part2: parse -> intersection -> length
