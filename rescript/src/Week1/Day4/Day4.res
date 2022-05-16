open Belt

type passport = Map.String.t<string>

let parseChunk = line =>
  line
  ->Js.String2.replaceByRe(%re("/\\n/g"), " ")
  ->Js.String2.split(" ")
  ->Array.map(x => x->Js.String2.split(":"))
  ->Array.keepMap(x =>
    switch x {
    | [x, y] => Some(x, y)
    | _ => None
    }
  )
  ->Map.String.fromArray

let parse = fileContent => fileContent->Js.String2.split("\n\n")->Array.map(parseChunk)

let validateField = passportCandidate =>
  ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
  ->Array.keep(x => passportCandidate->Map.String.has(x))
  ->Array.length == 7

let validate = passportCandidates => {
  let validateNDigitNum = (value, n) =>
    Js.Re.test_(Js.Re.fromString("\d{" ++ n->Int.toString ++ "}"), value)
  let validateFieldRule = (map, key, predicate) =>
    map->Map.String.get(key)->Option.map(predicate)->Option.getWithDefault(false)

  let validateDigitAndRange = (n, (min, max), value) =>
    [
      value->validateNDigitNum(n),
      value
      ->MSUtil.String.parseInt(10)
      ->Option.map(x => x->MSUtil.Math.Int.isInRange(min, max))
      ->Option.getWithDefault(false),
    ]->Array.reduce(true, (acc, x) => acc && x)

  let validateBirthYear = map =>
    map->validateFieldRule("byr", validateDigitAndRange(4, (1920, 2002)))

  let validateIssueYear = map =>
    map->validateFieldRule("iyr", validateDigitAndRange(4, (2010, 2020)))

  let validateExpirationYear = map =>
    map->validateFieldRule("eyr", validateDigitAndRange(4, (2020, 2030)))

  let validateHeight = map => {
    let validateByUnitAndRange = (value, unit, (min, max)) =>
      value
      ->Js.String2.replaceByRe(Js.Re.fromString(unit), "")
      ->MSUtil.String.parseInt(10)
      ->Option.map(x => x->MSUtil.Math.Int.isInRange(min, max))
      ->Option.getWithDefault(false)

    map->validateFieldRule("hgt", value =>
      switch value {
      | x if %re("/(\d+)cm/")->Js.Re.test_(x) => x->validateByUnitAndRange("cm", (150, 193))
      | x if %re("/(\d+)in/")->Js.Re.test_(x) => x->validateByUnitAndRange("in", (59, 76))
      | _ => false
      }
    )
  }

  let validateHairColor = map =>
    map->validateFieldRule("hcl", x => %re("/#[0-9a-f]{6}/")->Js.Re.test_(x))

  let validateEyeColor = map =>
    map->validateFieldRule("ecl", x =>
      switch x {
      | "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" => true
      | _ => false
      }
    )

  let validatePassportID = map =>
    map->validateFieldRule("pid", x => %re("/[0-9]{9}/")->Js.Re.test_(x))

  passportCandidates->Array.keep(x =>
    [
      validateField,
      validateBirthYear,
      validateIssueYear,
      validateExpirationYear,
      validateHeight,
      validateHairColor,
      validateEyeColor,
      validatePassportID,
    ]
    ->Array.map(y => y(x))
    ->Array.reduce(true, (acc, x) => acc && x)
  )
}

let goal1 = filePath => filePath->MSUtil.FileReader.readAllFile->parse->Array.keep(validateField)

let goal2 = filePath => filePath->MSUtil.FileReader.readAllFile->parse->validate

"input/Week1/Year2020Day4.sample2.txt"->goal2->Array.length->Js.log
// "input/Week1/Year2020Day4.sample3.txt"->goal2->Js.log
// "input/Week1/Year2020Day4.sample4.txt"->goal2->Js.log
