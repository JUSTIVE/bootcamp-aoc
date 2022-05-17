open Belt


module Passport = {

  
  type naivePassport = {
    byr: string,
    iyr: string,
    eyr: string,
    hgt: string,
    hcl: string,
    ecl: string,
    pid: string,
    cid: option<string>,
  }
  
  type rec strictPassport = {
    byr: int,
    iyr: int,
    eyr: int,
    hgt: height,
    hcl: hex,
    ecl: ecl,
    pid: nineNumericChars,
    cid: option<string>,
  }
  and height = In(int) | Cm(int)
  and hex = Hex(string)
  and ecl =
    | Amb
    | Blu
    | Brn
    | Gry
    | Grn
    | Hzl
    | Oth
  and nineNumericChars = string

  type rec passport = 
    | NaivePassport(naivePassport)
    | StrictPassport(strictPassport)

  // let make 
}

// and ecl = Colors...

// type rec passport = {
//   birthdayYear: birthdayYear,
//   issueYear: issueYear,
//   expirationYear: expirationYear,
//   height: height,
//   hairColor: hairColor,
//   eyeColor: eyeColor,
//   passportId: passportId,
//   countryId: option<countryId>,
// }
// and birthdayYear = Byr(int) | Byr_Naive
// and issueYear = Iyr(int) | Iyr_Naive
// and expirationYear = Eyr(int) | Eyr_Naive
// and height = In(int) | Cm(int) | Hgt_Naive
// and hairColor = Hcl(string) | Hcl_Naive
// and eyeColor = Ecl(string) | Ecl_Naive
// and passportId = Pid(string) | Pid_Naive
// and countryId = Cid(string)

// module type Comparable = {
//   type t
//   let cmp
// }
// module Map = (C: Comparable) => {
// C.t, C.cmp
// }

// module type Passport = {type t = passport, let make: string => t}
//
// module Parser = (A: A) = { -> 재사용성을 고려해보자.
//   let parse = str => str->A.make
// }
//
// module Passport = {
//   type t = { ... }
// }
//
// module PassportParser = Parser(Passport)
//
// PassportParser.parse(`hgt: "192 ..."`): string => Passport.t
type rec parserT<'a, 'b> = {
  re: Js.Re.t,
  kind: parserValueKind<'a, 'b>,
  strict: bool,
}
and parserValueKind<'a, 'b> =
  | NumericParser(numericParser)
  | StringParser(stringParser)
// | UnitParser(unitParser<'a, 'b>)
and numericParser = {numericRules: array<numericParserRule>}
and numericParserRule =
  | LengthConstraint(int)
  | RangeConstraint(int, int)
and stringParser = {stringRules: array<stringParserRule>}
and stringParserRule =
  | LengthConstraint(int)
  | KindConstraint(array<string>)
// and unitParser<'a, 'b> = {unitRules: array<unitParserRule<'a, 'b>>}
// and unitParserRule<'a, 'b> = {unitGen: 'a => 'b}

type parserResult =
  | Int(int)
  | String(string)
  | Something

let parsePassport = (fileContent, strict) => {
  let parseChunk = (line, strict) => {
    let getValueWithRe = (line, re) =>
      re
      ->Js.Re.exec_(line)
      ->Option.map(Js.Re.captures)
      ->Option.flatMap(x => x->Array.get(1))
      ->Option.flatMap(Js.Nullable.toOption)

    let parseWithParser = (line, parser) => {
      let parseNumberConstraint = (value, numericParserRule) =>
        switch numericParserRule {
        | LengthConstraint(length) if value->MSUtil.Math.Int.length == length => Some(value)
        | RangeConstraint(min, max) =>
          switch value {
          | x if x->MSUtil.Math.Int.isInRange((min, max)) => Some(x)
          | _ => None
          }
        | _ => None
        }

      let parseStringConstraint = (value, stringParserRule: stringParserRule) =>
        switch stringParserRule {
        | LengthConstraint(length) if value->Js.String.length == length => Some(value)
        | KindConstraint(kinds) if kinds->MSUtil.Array.has(value) => Some(value)
        | _ => None
        }
      let advancedParse = (value, parser) => {
        switch parser.kind {
        | NumericParser({numericRules}) =>
          numericRules
          ->Array.map(x => parseNumberConstraint(_, x))
          ->Array.reduce(value->Option.flatMap(x => x->MSUtil.String.parseInt(10)), (x, y) =>
            x->Option.flatMap(x => y(x))
          )
          ->Option.map(x => x->Int)
        | StringParser({stringRules}) =>
          stringRules
          ->Array.map(x => parseStringConstraint(_, x))
          ->Array.reduce(value, (x, y) => x->Option.flatMap(x => y(x)))
          ->Option.map(x => x->String)
        | _ => None
        }
      }
      switch parser.strict {
      | true => line->getValueWithRe(parser.re)->advancedParse(parser)
      | false => line->getValueWithRe(parser.re)->Option.map(_ => Something)
      }
    }

    let naiveRe = key => Js.Re.fromString("/" ++ key ++ ":(\w+)/")

    let birthdayYear = line =>
      line
      ->parseWithParser({
        re: naiveRe("byr"),
        strict: strict,
        kind: NumericParser({
          numericRules: [LengthConstraint(4), RangeConstraint(1920, 2002)],
        }),
      })
      ->Option.flatMap(x =>
        switch x {
        | Int(x) => x->Byr->Some
        | Something => Byr_Naive->Some
        | _ => None
        }
      )

    let issueYear = line =>
      line
      ->parseWithParser({
        re: naiveRe("iyr"),
        strict: strict,
        kind: NumericParser({
          numericRules: [LengthConstraint(4), RangeConstraint(2010, 2020)],
        }),
      })
      ->Option.flatMap(x =>
        switch x {
        | Int(x) => x->Iyr->Some
        | Something => Iyr_Naive->Some
        | _ => None
        }
      )

    let expirationYear = line =>
      line
      ->parseWithParser({
        re: naiveRe("eyr"),
        strict: strict,
        kind: NumericParser({
          numericRules: [LengthConstraint(4), RangeConstraint(2020, 2030)],
        }),
      })
      ->Option.flatMap(x =>
        switch x {
        | Int(x) => x->Eyr->Some
        | Something => Eyr_Naive->Some
        | _ => None
        }
      )

    let height = line => {
      let parseWithRange = (x, (min, max), re, gen) =>
        switch x->getValueWithRe(re)->Option.flatMap(x => x->MSUtil.String.parseInt(10)) {
        | Some(x) if x->MSUtil.Math.Int.isInRange((min, max)) => Some(gen(x))
        | _ => None
        }
      let parseHeight = value =>
        switch value {
        | x if %re("/(\d+)cm/")->Js.Re.test_(x) =>
          x->parseWithRange((150, 193), %re("/(\d+)cm/"), x => x->Cm)
        | x if %re("/(\d+)in/")->Js.Re.test_(x) =>
          x->parseWithRange((59, 76), %re("/(\d+)in/"), x => x->In)
        | _ => None
        }

      line
      ->parseWithParser({
        re: naiveRe("hgt"),
        strict: strict,
        kind: StringParser({
          stringRules: [LengthConstraint(2), KindConstraint(["cm", "in"])],
        }),
      })
      ->Option.flatMap(x =>
        switch x {
        | Int(_) => None
        | String(x) => parseHeight(x)
        | Something => Hgt_Naive->Some
        }
      )

      // line->parseWithStrict(strict, "hgt", %re("/hgt:(\w+)/"), _ => Hgt_Naive, parseHeight)
    }

    let hairColor = line =>
      line
      ->parseWithParser({
        re: naiveRe("hcl"),
        strict: strict,
        kind: StringParser({
          stringRules: [LengthConstraint(7)],
        }),
      })
      ->Option.flatMap(x =>
        switch x {
        | String(x) => x->Hcl->Some
        | Something => Hcl_Naive->Some
        | _ => None
        }
      )

    let eyeColor = line =>
      line
      ->parseWithParser({
        re: naiveRe("ecl"),
        strict: strict,
        kind: StringParser({
          stringRules: [KindConstraint(["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])],
        }),
      })
      ->Option.flatMap(x =>
        switch x {
        | String(x) => x->Ecl->Some
        | Something => Ecl_Naive->Some
        | _ => None
        }
      )

    let passportId = line =>
      line
      ->parseWithParser({
        re: naiveRe("pid"),
        strict: strict,
        kind: StringParser({
          stringRules: [LengthConstraint(9)],
        }),
      })
      ->Option.flatMap(x =>
        switch x {
        | String(x) => x->Pid->Some
        | Something => Pid_Naive->Some
        | _ => None
        }
      )

    let countryId = line =>
      line
      ->parseWithParser({
        re: naiveRe("cid"),
        strict: false,
        kind: StringParser({
          stringRules: [LengthConstraint(9)],
        }),
      })
      ->Option.flatMap(x =>
        switch x {
        | String(x) => x->Cid->Some
        | Something => ""->Cid->Some
        | _ => None
        }
      )

    (
      line->birthdayYear,
      line->issueYear,
      line->expirationYear,
      line->height,
      line->hairColor,
      line->eyeColor,
      line->passportId,
      line->countryId,
    )->Js.log

    switch (
      line->birthdayYear,
      line->issueYear,
      line->expirationYear,
      line->height,
      line->hairColor,
      line->eyeColor,
      line->passportId,
      line->countryId,
    ) {
    | (
        Some(birthdayYear),
        Some(issueYear),
        Some(expirationYear),
        Some(height),
        Some(hairColor),
        Some(eyeColor),
        Some(passportId),
        countryId,
      ) =>
      Some({
        birthdayYear: birthdayYear,
        issueYear: issueYear,
        expirationYear: expirationYear,
        height: height,
        hairColor: hairColor,
        eyeColor: eyeColor,
        passportId: passportId,
        countryId: countryId,
      })
    | _ => None
    }
  }
  fileContent->Js.String2.split("\n\n")->Array.map(line => line->parseChunk(strict))
}

let countPassport = Array.length

let goal1 = filePath =>
  filePath->MSUtil.FileReader.readAllFile->parsePassport(false)->Array.keep(Option.isSome)

let goal2 = filePath =>
  filePath->MSUtil.FileReader.readAllFile->parsePassport(true)->Array.keep(Option.isSome)

// "input/Week1/Year2020Day4.sample2.txt"->goal2->countPassport->Js.log
// "input/Week1/Year2020Day4.sample3.txt"->goal2->countPassport->Js.log
"input/Week1/Year2020Day4.sample1.txt"->goal2->countPassport->Js.log
