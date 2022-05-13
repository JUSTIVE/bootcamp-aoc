open Belt

type slant = {
  dx: int,
  dy: int,
}

type gridSize = {
  width: int,
  height: int,
}

type grid = {
  cells: array<string>,
  size: gridSize,
}

let slant_ = (dx, dy) => {
  {
    dx: dx,
    dy: dy,
  }
}

let gridSize_ = cells => {
  {
    width: cells->Array.length,
    height: cells->Array.getExn(0)->Js.String.length,
  }
}

let grid_ = filePath => {
  let cells = filePath->FileReader.readFileLine
  {
    cells: cells,
    size: gridSize_(cells),
  }
}

let nthCharFromStringPattern = (target, n) => {
  target->Js.String2.charAt(mod(n, target->Js.String2.length))
}

let potentialCollisionCount = (slant,grid) => {
  grid.cells
  ->Array.keepWithIndex((_, i) => mod(i, slant.dy) === 0)
  ->Array.mapWithIndex((i, x) => x->nthCharFromStringPattern(i * slant.dx))
  ->Array.keep(x => x === "#")
  ->Array.length
}
