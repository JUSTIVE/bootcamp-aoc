let expect = (name, expected, actual) => {
  switch actual {
  | x if x === expected => `${name} -- Pass ✅`
  | __ => `${name} -- Fail ❌`
  }->Js.log
}