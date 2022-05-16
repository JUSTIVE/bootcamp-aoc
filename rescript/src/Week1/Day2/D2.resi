type rule
type parsedLine

let parse: string => option<parsedLine>

let validate1: parsedLine => bool

let validate2: parsedLine => bool

let goal1 :string=>unit

let goal2 :string=>unit

// 모듈 펑터
// Belt.Map 모듈 펑터 여서 사용하는 법 
