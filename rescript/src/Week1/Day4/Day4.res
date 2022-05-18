open Belt
open MSUtil.Parser

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

  // type rec passport =
  //   | NaivePassport(naivePassport)
  //   | StrictPassport(strictPassport)

  let transform: naivePassport => option<strictPassport> = (naivePassport: naivePassport) => {
    let legnthAndRangeParser = (value, length, (min, max)) =>
      ConstraintParser.NumericParser({
        radix: 10,
        numericRules: [NumericLength(length), NumericRange(min, max)],
      })->ConstraintParser.parse(value)

    let byr = legnthAndRangeParser(naivePassport.byr, 4, (1920, 2002))

    let iyr = legnthAndRangeParser(naivePassport.iyr, 4, (2010, 2020))

    let eyr = legnthAndRangeParser(naivePassport.eyr, 4, (2020, 2030))

    let rangeParser = (value, (min, max), gen) =>
      ConstraintParser.NumericParser({
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
        | [value, "cm"] => value->rangeParser((150, 193), x => x->Cm)
        | [value, "in"] => value->rangeParser((59, 76), x => x->In)
        | _ => None
        }
      )

    let hcl =
      ConstraintParser.StringParser({
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
      ConstraintParser.StringParser({
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

    let pid = ConstraintParser.StringParser({
      stringRules: [StringLength(9)],
    })->ConstraintParser.parse(naivePassport.pid)

    let cid = naivePassport.cid
    (
      naivePassport.byr,
      naivePassport.iyr,
      naivePassport.eyr,
      naivePassport.hgt,
      naivePassport.hcl,
      naivePassport.ecl,
      naivePassport.pid,
      naivePassport.cid,
    )->Js.log
    (byr, iyr, eyr, hgt, hcl, ecl, pid, cid)->Js.log

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

    let byr = "byr"->naiveParser(stringChunk)
    let iyr = "iyr"->naiveParser(stringChunk)
    let eyr = "eyr"->naiveParser(stringChunk)
    let hgt = "hgt"->naiveParser(stringChunk)
    let hcl = "hcl"->naiveParser(stringChunk)
    let ecl = "ecl"->naiveParser(stringChunk)
    let pid = "pid"->naiveParser(stringChunk)
    let cid = "cid"->naiveParser(stringChunk)

    switch (byr, iyr, eyr, hgt, hcl, ecl, pid, cid) {
    | (Some(byr), Some(iyr), Some(eyr), Some(hgt), Some(hcl), Some(ecl), Some(pid), cid) =>
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

// "input/Week1/Year2020Day4.sample2.txt"->goal1->Array.length->Js.log
"input/Week1/Year2020Day4.sample2.txt"->goal2->Array.length->Js.log
// "input/Week1/Year2020Day4.sample3.txt"->goal2->countPassport->Js.log
// "input/Week1/Year2020Day4.sample1.txt"->goal2->countPassport->Js.log
