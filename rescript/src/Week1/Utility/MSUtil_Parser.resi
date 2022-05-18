module RegexGroupParser: {
  type t

  let make: (Js.Re.t, array<int>) => t

  let parse: (t, string) => option<array<string>>
}

module ConstraintParser: {
  type parseResult =
    | ParseResInt(int)
    | ParseResString(string)
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
  let parse: (t, string) => option<parseResult>
}
