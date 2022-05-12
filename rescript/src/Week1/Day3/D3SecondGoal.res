open Belt

type slant = {
  dx: int,
  dy: int,
}

type size = {
    width:int,
    height:int
}

type grid = {
    cells: array<string>,
    size: size
}

let solution = (slant, filePath) => {
  let genGrid = filePath => {
    let cells =
        filePath
        ->Node.Fs.readFileAsUtf8Sync
        ->Js_string2.split("\n")

    {
      cells
    }
  }
  genGrid(filePath)
  ->Array.keepWithIndex((_, i) => mod(i, slant.dy) === 0)
  ->Array.mapWithIndex((i,x) => x->[mod()])
}

solution({dx: 3, dy: 1},"input/Week1/Year2020Day3.sample.txt")->Js.log
