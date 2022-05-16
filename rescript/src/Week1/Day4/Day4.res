open Belt

type rec passport = {
  birthdayYear: birthdayYear,
  issueYear: issueYear,
  expirationYear: expirationYear,
  height: height,
  hairColor: hairColor,
  eyeColor: eyeColor,
  passportId: passportId,
}
and birthdayYear = Byr(int)
and issueYear = Iyr(int)
and expirationYear = Eyr(int)
and height = In(int) | Cm(int)
and hairColor = Hcl(string)
and eyeColor = Ecl(string)
and passportId = Pid(string)

let parsePassport = {
  let parseChunk = line => {
    let getValueWithRe = (line, re) =>
      re
      ->Js.Re.exec_(line)
      ->Option.map(Js.Re.captures)
      ->Option.map(x => x->Array.map(x => x->Js.Nullable.toOption))
      ->Option.flatMap(x => x->Array.get(1))
      ->Option.getWithDefault(None)

    let parseWithDigitAndRange = ((min, max), gen, re) =>
      switch line->getValueWithRe(re)->Option.flatMap(x => x->MSUtil.String.parseInt(10)) {
      | Some(x) if x->MSUtil.Math.Int.isInRange(min, max) => Some(gen(x))
      | _ => None
      }

    let birthdayYear = parseWithDigitAndRange((1920, 2002), x => x->Byr, %re("/byr:(\d+)/"))

    let issueYear = parseWithDigitAndRange((2010, 2020), x => x->Iyr, %re("/iyr:(\d+)/"))

    let expirationYear = parseWithDigitAndRange((2020, 2030), x => x->Eyr, %re("/eyr:(\d+)/"))

    let height = {
      let parseWithRange = (x, (min, max), re, gen) =>
        switch x->getValueWithRe(re)->Option.flatMap(x => x->MSUtil.String.parseInt(10)) {
        | Some(x) if x->MSUtil.Math.Int.isInRange(min, max) => Some(gen(x))
        | _ => None
        }

      line
      ->getValueWithRe(%re("/hgt:(\w+)/"))
      ->Option.flatMap(x =>
        switch x {
        | x if %re("/(\d+)cm/")->Js.Re.test_(x) =>
          x->parseWithRange((150, 193), %re("/(\d+)cm/"), x => x->Cm)
        | x if %re("/(\d+)in/")->Js.Re.test_(x) =>
          x->parseWithRange((59, 76), %re("/(\d+)in/"), x => x->In)
        | _ => None
        }
      )
    }

    let hairColor = line->getValueWithRe(%re("/hcl:(#[0-9a-f]{6})/"))->Option.map(x => x->Hcl)

    let eyeColor =
      line->getValueWithRe(%re("/ecl:(amb|blu|brn|gry|grn|hzl|oth)/"))->Option.map(x => x->Ecl)

    let passportId =
      line
      ->getValueWithRe(%re("/pid:(\d+)/"))
      ->Option.flatMap(x => x->getValueWithRe(%re("/^(\d{9})$/"))->Option.map(x => x->Pid))

    (birthdayYear, issueYear, expirationYear, height, hairColor, eyeColor, passportId)->Js.log

    switch (birthdayYear, issueYear, expirationYear, height, hairColor, eyeColor, passportId) {
    | (
        Some(birthdayYear),
        Some(issueYear),
        Some(expirationYear),
        Some(height),
        Some(hairColor),
        Some(eyeColor),
        Some(passportId),
      ) =>
      Some({
        birthdayYear: birthdayYear,
        issueYear: issueYear,
        expirationYear: expirationYear,
        height: height,
        hairColor: hairColor,
        eyeColor: eyeColor,
        passportId: passportId,
      })
    | _ => None
    }
  }
  fileContent => fileContent->Js.String2.split("\n\n")->Array.map(parseChunk)
}

let countPassport = Array.length

let goal1 = filePath =>
  filePath->MSUtil.FileReader.readAllFile->parsePassport->Array.keep(Option.isSome)

let goal2 = filePath =>
  filePath->MSUtil.FileReader.readAllFile->parsePassport->Array.keep(Option.isSome)

// "input/Week1/Year2020Day4.sample2.txt"->goal2->countPassport->Js.log
// "input/Week1/Year2020Day4.sample3.txt"->goal2->countPassport->Js.log
"input/Week1/Year2020Day4.sample2.txt"->goal2->countPassport->Js.log
