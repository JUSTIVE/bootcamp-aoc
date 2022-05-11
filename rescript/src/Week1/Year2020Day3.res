let input = Node.Fs.readFileAsUtf8Sync("input/Week1/Year2020Day3.sample.txt")

type size = {
  width: int,
  height: int,
}



let pattern: array<string> = Js_string.split("\n", input)
let patternMeta = {
  width: pattern->Array.length,
  height: pattern[0]->String.length,
}

let isTree = (x, y) =>
  switch String.get(pattern[y], x) {
  | '#' => 1
  | __ => 0
  }

let rec countTree = (state, currentRow) =>
  switch currentRow {
  | x if x === patternMeta.height => state
  | __ => {
      let xCoord = mod(3 * currentRow, patternMeta.width)
      let newState = state + isTree(currentRow, xCoord)
      countTree(newState, currentRow + 1)
    }
  }

countTree(0, 0)->Js.log