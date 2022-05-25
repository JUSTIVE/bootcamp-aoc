let xor = (a, b) => (a || b) && !(a && b)

let xnor = (a, b) => !xor(a, b)