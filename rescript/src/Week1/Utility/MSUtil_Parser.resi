module ParserResult: {
  type t =
    | ParseResInt(int)
    | ParseResString(string)

  // let flatMap:(t,t=>'a) => 'a
}
module RegexGroupParser: {
  type t

  let make: (Js.Re.t, array<int>) => t
  let parse: (t, string) => option<array<ParserResult.t>>
}

module ConstraintParser: {
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

  let parseNumberConstraint: (int, numericParserRule) => option<int>
  let parseStringConstraint: (string, stringParserRule) => option<string>
  let parse: (t, string) => option<ParserResult.t>
}
