type seat
type arbitraryBinaryPattern

let stringToBinary: (string, arbitraryBinaryPattern) => string
let processSeatID: seat => int
let parseBoardingPass: string => option<seat>
let findMissingSeat: array<int> => int
let generateSeatID:(array<string>)=>array<int>