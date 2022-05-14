module Int = {
  let sum = value => value->Belt.Array.reduce(0, (a, b) => a + b)
  let max = value => value->Belt.Array.reduce(0, (a, b) => a > b ? a : b)
  let ma = (x, a, b) => x * a + b
  let product = values => values->Belt.Array.reduce(1, (a, b) => a * b)
  let isInRange = (value, lowerBound, upperBound) => value >= lowerBound && value <= upperBound
}

module Float = {
  let sum = value => value->Belt.Array.reduce(0.0, (a, b) => a +. b)
  let max = value => value->Belt.Array.reduce(0.0, (a, b) => a > b ? a : b)
  let ma = (x, a, b) => x *. a +. b
  let product = values => values->Belt.Array.reduce(1.0, (a, b) => a *. b)
  let isInRange = (value, lowerBound, upperBound) => value >= lowerBound && value <= upperBound
}
