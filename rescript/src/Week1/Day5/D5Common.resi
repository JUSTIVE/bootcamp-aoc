type seat
type arbitraryBinaryMatcherPattern

let seat:(int,int)=>seat
let arbitraryBinaryMatcherPattern:(string,string)=>arbitraryBinaryMatcherPattern
let applyMapper: (arbitraryBinaryMatcherPattern, string) => int
let parseBinaryString: (string, arbitraryBinaryMatcherPattern) => int
let processSeatID: seat => int
let parseBoardingPass: string => seat
let takeFirst: (array<'a>, 'a) => 'a
let findMissingSeat: array<int> => int
let generateSeatID:(array<string>)=>array<int>