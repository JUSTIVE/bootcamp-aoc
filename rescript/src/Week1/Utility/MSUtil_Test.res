let expectInt = (name, expected, actual) => {
  switch actual {
  | x if x == expected => `${name} -- Pass ✅`
  | __ =>
    j`${name} -- Fail ❌ expected ${expected->Belt.Int.toString} but got ${actual->Belt.Int.toString}`
  }->Js.log
}
