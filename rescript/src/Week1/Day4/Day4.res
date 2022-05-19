open Belt
open MSUtil.Parser
open MSUtil.Parser.ConstraintParser

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

  let transform: naivePassport => option<strictPassport> = (naivePassport: naivePassport) => {
    let legnthAndRangeParser = (value, length, (min, max)) =>
      NumericParser({
        radix: 10,
        numericRules: [NumericLength(length), NumericRange(min, max)],
      })->ConstraintParser.parse(value)

    let byr = legnthAndRangeParser(naivePassport.byr, 4, (1920, 2002))
    let iyr = legnthAndRangeParser(naivePassport.iyr, 4, (2010, 2020))
    let eyr = legnthAndRangeParser(naivePassport.eyr, 4, (2020, 2030))

    let rangeParser = (value, (min, max), gen) =>
      NumericParser({
        radix: 10,
        numericRules: [NumericRange(min, max)],
      })
      ->ConstraintParser.parse(value)
      ->Option.flatMap(x =>
        switch x {
        | ParseResInt(x) => x->gen->Some
        | _ => None
        }
      )

    let hgt =
      RegexGroupParser.make(%re("/^(\d+)(cm|in)$/"), [1, 2])
      ->RegexGroupParser.parse(naivePassport.hgt)
      ->Option.flatMap(x =>
        switch x {
        | [ParseResString(value), ParseResString("cm")] =>
          value->rangeParser((150, 193), x => x->Cm)
        | [ParseResString(value), ParseResString("in")] => value->rangeParser((59, 76), x => x->In)
        | _ => None
        }
      )

    let hcl =
      StringParser({
        stringRules: [StringLength(7), StringStartsWith("#")],
      })
      ->ConstraintParser.parse(naivePassport.hcl)
      ->Option.flatMap(x =>
        switch x {
        | ParseResString(x) => Some(Hex(x))
        | _ => None
        }
      )

    let ecl =
      StringParser({
        stringRules: [StringKind(["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])],
      })
      ->ConstraintParser.parse(naivePassport.ecl)
      ->Option.flatMap(x =>
        switch x {
        | ParseResString(x) =>
          switch x {
          | "amb" => Some(Amb)
          | "blu" => Some(Blu)
          | "brn" => Some(Brn)
          | "gry" => Some(Gry)
          | "grn" => Some(Grn)
          | "hzl" => Some(Hzl)
          | "oth" => Some(Oth)
          | _ => None
          }
        | _ => None
        }
      )

    let pid = StringParser({
      stringRules: [StringLength(9)],
    })->ConstraintParser.parse(naivePassport.pid)

    let cid = naivePassport.cid

    switch (byr, iyr, eyr, hgt, hcl, ecl, pid, cid) {
    | (
        Some(ParseResInt(byr)),
        Some(ParseResInt(iyr)),
        Some(ParseResInt(eyr)),
        Some(height),
        Some(hcl),
        Some(ecl),
        Some(ParseResString(pid)),
        cid,
      ) =>
      {
        byr: byr,
        iyr: iyr,
        eyr: eyr,
        hgt: height,
        hcl: hcl,
        ecl: ecl,
        pid: pid,
        cid: cid,
      }->Some
    | _ => None
    }
  }

  let make: string => option<naivePassport> = stringChunk => {
    let naiveParser = (key, string) =>
      Js.Re.fromString("" ++ key ++ ":([\w#]+)")
      ->RegexGroupParser.make([1])
      ->RegexGroupParser.parse(string)
      ->Option.flatMap(x => x->Array.get(0))

    switch (
      "byr"->naiveParser(stringChunk),
      "iyr"->naiveParser(stringChunk),
      "eyr"->naiveParser(stringChunk),
      "hgt"->naiveParser(stringChunk),
      "hcl"->naiveParser(stringChunk),
      "ecl"->naiveParser(stringChunk),
      "pid"->naiveParser(stringChunk),
      switch "cid"->naiveParser(stringChunk){
      | Some(ParseResString(x)) => Some(x)
      | _ => None
      },
    ) {
    | (
        Some(ParseResString(byr)),
        Some(ParseResString(iyr)),
        Some(ParseResString(eyr)),
        Some(ParseResString(hgt)),
        Some(ParseResString(hcl)),
        Some(ParseResString(ecl)),
        Some(ParseResString(pid)),
        cid,
      ) =>
      {
        byr: byr,
        iyr: iyr,
        eyr: eyr,
        hgt: hgt,
        hcl: hcl,
        ecl: ecl,
        pid: pid,
        cid: cid,
      }->Some
    | _ => None
    }
  }
}

let goal1 = filePath =>
  filePath
  ->MSUtil.FileReader.readAllFile
  ->Js.String2.split("\n\n")
  ->Array.map(x => x->Js.String2.replaceByRe(%re("/\\n/g"), " "))
  ->Array.map(Passport.make)
  ->Array.keep(Option.isSome)

let goal2 = filePath =>
  filePath->goal1->Array.map(x => x->Option.flatMap(Passport.transform))->Array.keep(Option.isSome)

"input/Week1/Year2020Day4.sample2.txt"->goal1->Array.length->Js.log
"input/Week1/Year2020Day4.sample2.txt"->goal2->Array.length->Js.log
// "input/Week1/Year2020Day4.sample3.txt"->goal2->countPassport->Js.log
// "input/Week1/Year2020Day4.sample1.txt"->goal2->countPassport->Js.log
