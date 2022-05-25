module ParserResult = {
  type t =
    | ParseResInt(int)
    | ParseResString(string)
  
}
module RegexGroupParser = {
  type t = {
    re: Js.Re.t,
    groupNumber: array<int>,
  }
  let make = (re, groupNumber): t => {
    re: re,
    groupNumber: groupNumber,
  }
  let parse = (t, value) => {
    t.re
    ->Js.Re.exec_(value)
    ->Belt.Option.map(Js.Re.captures)
    ->Belt.Option.map(Js.Array.map(Js.Nullable.toOption))
    ->Belt.Option.flatMap(MSUtil_Array.everyO)
    ->Belt.Option.flatMap(x => x->MSUtil_Array.take(t.groupNumber))
    ->Belt.Option.map(x => x->Belt.Array.map(x => ParserResult.ParseResString(x)))
  }
}

module ConstraintParser = {
  type stringParserRule =
    | StringLength(int)
    | StringKind(array<string>)
    | StringStartsWith(string)
    | StringContains(string)

  type stringParser = {stringRules: array<stringParserRule>}
  type numericParserRule =
    | NumericLength(int)
    | NumericRange(int, int)
  type numericParser = {
    radix: int,
    numericRules: array<numericParserRule>,
  }
  type t =
    | NumericParser(numericParser)
    | StringParser(stringParser)

  let parseNumberConstraint = (value, numericParserRule) =>
    switch numericParserRule {
    | NumericLength(length) if value->MSUtil_Math.Int.length == length => Some(value)
    | NumericRange(min, max) =>
      switch value {
      | x if x->MSUtil_Math.Int.isInRange((min, max)) => Some(x)
      | _ => None
      }
    | _ => None
    }

  let parseStringConstraint = (value, stringParserRule: stringParserRule) =>
    switch stringParserRule {
    | StringLength(length) if value->Js.String.length == length => Some(value)
    | StringKind(kinds) if kinds->MSUtil_Array.has(value) => Some(value)
    | StringStartsWith(startsWith) if value->Js.String2.startsWith(startsWith) => Some(value)
    | _ => None
    }

  let parse = (parser, value) => {
    switch parser {
    | NumericParser({radix, numericRules}) =>
      numericRules
      ->Belt.Array.map(x => parseNumberConstraint(_, x))
      ->Belt.Array.reduce(value->MSUtil_String.parseInt(radix), (x, y) =>
        x->Belt.Option.flatMap(x => y(x))
      )
      ->Belt.Option.map(x => x->ParserResult.ParseResInt)
    | StringParser({stringRules}) =>
      stringRules
      ->Belt.Array.map(x => parseStringConstraint(_, x))
      ->Belt.Array.reduce(Some(value), (x, y) => x->Belt.Option.flatMap(x => y(x)))
      ->Belt.Option.map(x => x->ParserResult.ParseResString)
    }
  }
}
