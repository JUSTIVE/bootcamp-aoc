open D3Common

let solution = (slant, filePath) => slant->potentialCollisionCount(grid_(filePath))

slant_(3, 1)->solution("input/Week1/Year2020Day3.sample.txt")->Js.log
