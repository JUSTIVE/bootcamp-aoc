import fs from 'fs'

function pipe<A, B>(a: A, ab: (a: A) => B): B
function pipe<A, B, C>(a: A, ab: (a: A) => B, bc: (b: B) => C): C
function pipe<A, B, C, D>(a: A, ab: (a: A) => B, bc: (b: B) => C, cd: (c: C) => D): D
function pipe(
  a: unknown,
  ab?: Function,
  bc?: Function,
  cd?: Function
): unknown {
  switch (arguments.length) {
    case 1:
      return a;
    case 2:
      return ab!(a);
    case 3:
      return bc!(ab!(a));
    case 4:
      return cd!(bc!(ab!(a)));
  }
}

type TypeApplier<Key extends string, T extends string, Value> =
  Record<Key, T> & { value: Value }

type OptionalKind =
  "Some"
  | "None"

type OptionalTypeApplier<K extends OptionalKind, V> =
  TypeApplier<"optionalType", K, V>

type Some<T> = OptionalTypeApplier<"Some", T>
type None = OptionalTypeApplier<"None", null>

type Optional<T> = Some<T> | None
const Optional = {
  some: <T>(value: T) => ({
    optionalType: 'Some',
    value
  }) as Some<T>,
  none: {optionalType: 'None', value: null} as None,
  optional: <T>(value: T | undefined | null) =>
    value === undefined || value === null
      ? Optional.none
      : Optional.some(value)
}

type CellKind = "Tree" | "Empty"
type CellTypeApplier<K extends CellKind, V> = TypeApplier<"cellType", K, V>

type Tree = CellTypeApplier<"Tree", "#">
type Empty = CellTypeApplier<"Empty", ".">

type Cell = Tree | Empty

type GridSize = {
  width: number,
  height: number
}

type GridMetaInfo = {
  size: GridSize
}

type Grid = {
  cells: Cell[][],
  meta: GridMetaInfo
}

type Coord = {
  x: number,
  y: number
}

function AccessGrid(coord: Coord, grid: Grid): Optional<Cell> {
  return Optional.optional(grid.cells[coord.y][coord.x])
}

type Slant = {
  dx: number,
  dy: number
}

function Grid_(cells: Cell[][]): Grid {
  function GridMetaInfo_(size: GridSize): GridMetaInfo {
    return {size}
  }

  function GridSize_(width: number, height: number): GridSize {
    return {width, height}
  }

  const meta = GridMetaInfo_(
    GridSize_(
      cells[0].length,
      cells.length))

  return {cells, meta}
}

function CellGrid_(input: string): Cell[][] {
  function CellRow_(input: string): Cell[] {
    function Cell_(input: string): Optional<Cell> {
      function Tree_(value: "#"): Tree {
        return {
          cellType: "Tree",
          value
        }
      }

      function Empty_(value: "."): Empty {
        return {
          cellType: "Empty",
          value
        }
      }

      switch (input) {
        case "#":
          return Optional.optional(Tree_(input))
        case ".":
          return Optional.optional(Empty_(input))
        default:
          return Optional.none
      }
    }

    return (
      ([...input]
        .map(Cell_)
        .filter(x => x.optionalType === 'Some') as Some<Cell>[])
        .map(x => x.value)
    )
  }

  return input.split('\n').map(CellRow_)
}

const range = (start: number, end: number, gap: number) =>
  Array.from(
    {length: Math.ceil(Math.max(0, end - start) / gap)},
    (_, i) => (i * gap) + start)

const multiply = (x: number, y: number) => x * y

function Slant_(dx: number, dy: number): Slant {
  return {dx, dy}
}

function solution(filePath: string): (slant: Slant) => number {
  function genGrid(filePath: string): Grid {
    return pipe(
      filePath,
      (x) => fs.readFileSync(x, 'utf-8'),
      CellGrid_,
      Grid_
    )
  }

  function resolve(grid: Grid): (slant: Slant) => number {
    return function (slant: Slant): number {
      return grid
        .cells
        .filter((_, i) => i % slant.dy === 0)
        .map((x, i) => x[(i * slant.dx) % grid.meta.size.width])
        .filter(x=>x)
        .filter(x => x.cellType === "Tree")
        .length
    }
  }

  return resolve(genGrid(filePath))
}

// input -> map -> process

// PPAP => Parse -> Process -> Aggregate -> Print


function E2E() {
  function expect<T>(name: string, input: T, oracle: T) {
    if (typeof input === 'number'
      || typeof input === 'string'
      || typeof input === 'boolean'
      || typeof input === 'undefined')
      if (input === oracle)
        console.log(`${name} -- Pass ✅`)
      else
        console.log(`${name} -- Fail ❌ : expected ${oracle} but received ${input}.`)

  }


  const grid = pipe(
    fs.readFileSync("input/Week1/Year2020Day3.sample.txt", "utf8"),
    CellGrid_,
    Grid_
  )
  expect(
    "example test",
    range(1, grid.meta.size.height, 1)
      .map(x => grid.cells[x][(3 * x) % grid.meta.size.width])
      .filter(x => x)
      .map(x => x.value).join(""),
    ".#.##.####"
  )
  expect(
    "range dy test",
    range(0, 5, 2),
    [0, 2, 4]
  )
}

// solution({dx:3,dy:1})
pipe([
    Slant_(1, 1),
    Slant_(3, 1),
    Slant_(5, 1),
    Slant_(7, 1),
    Slant_(1, 2),
  ]
    .map(solution("input/Week1/Year2020Day3.sample.txt"))
    .reduce(multiply),
  console.log
)
E2E()