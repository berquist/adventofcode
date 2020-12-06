import strutils
import ./aoc_utils
import unittest

type
  Slope = seq[string]
  Step = tuple
    x: int
    y: int
  SlopeState = object
    slope: Slope
    posx: int
    posy: int
    numHits: int

func newSlopeState(slope: Slope): SlopeState =
  SlopeState(slope: slope, posx: 0, posy: 0, numHits: 0)

func extendSlope(slope: Slope): Slope =
  ## Extend the slope to the right by copying the existing scope once.
  result = newSeq[string]()
  for row in slope:
    result.add(row & row)

func step(state: SlopeState, stepx, stepy: int): (SlopeState, bool) =
  ## Take a single step down the slope,
  ## - incrementing the current position,
  ## - marking if a tree or open snow was hit,
  ## - and incrementing the count of number of trees hit.
  let
    newx = state.posx + stepx
    newy = state.posy + stepy
  # Check if the new x position is in bounds. If not, extend the slope to the
  # side.
  var slope = state.slope
  if newx >= slope[0].len:
    slope = extendSlope(slope)
  # Check if the new y position is in bounds. If not, we are done.
  if newy >= slope.len:
    return (state, true)
  var numHits = state.numHits
  case slope[newy][newx]
    of '#':
      numHits += 1
    of '.':
      discard
    else:
      raise
  (SlopeState(slope: slope, posx: newx, posy: newy, numHits: numHits), false)

func run(state: SlopeState, stepx, stepy: int): int =
  ## Take single steps down the slope until the bottom is reached, returning
  ## how many trees have been hit.
  var
    atBottom = false
    stepState = state.deepCopy()
  while not atBottom:
    (stepState, atBottom) = stepState.step(stepx, stepy)
  stepState.numHits

proc runSlopes(state: SlopeState, steps: openArray[Step]): int =
  result = 1
  if steps.len == 0:
    return 0
  for step in steps:
    result *= state.run(step.x, step.y)

suite "day3":
  let
    example = """
                 ..##.......
                 #...#...#..
                 .#....#..#.
                 ..#.#...#.#
                 .#...##..#.
                 ..#.##.....
                 .#.#.#....#
                 .#........#
                 #.##...#...
                 #...##....#
                 .#..#...#.#
               """.unindent.strip
    exampleLines = example.splitlines()
  let state = newSlopeState(exampleLines)
  check: state.run(3, 1) == 7
  let steps = [(x: 1, y: 1),
               (x: 3, y: 1),
               (x: 5, y: 1),
               (x: 7, y: 1),
               (x: 1, y: 2)]
  check: state.run(1, 1) == 2
  check: state.run(5, 1) == 3
  check: state.run(7, 1) == 4
  check: state.run(1, 2) == 2
  check: state.runSlopes(steps) == 336

when isMainModule:
  let
    inputLines = readAllLines("day3_input.txt")
  var state = newSlopeState(inputLines)
  echo "part 1: ", state.run(3, 1)
  let steps = [(x: 1, y: 1),
               (x: 3, y: 1),
               (x: 5, y: 1),
               (x: 7, y: 1),
               (x: 1, y: 2)]
  echo "part 2: ", state.runSlopes(steps)
