type seat
type arbitraryBinaryPattern

let seat:(int,int)=>seat
let arbitraryBinaryPattern:(string,string)=>arbitraryBinaryPattern
let stringToBinary: (string, arbitraryBinaryPattern) => string
let processSeatID: seat => int
let parseBoardingPass: string => seat
let findMissingSeat: array<int> => int
let generateSeatID:(array<string>)=>array<int>