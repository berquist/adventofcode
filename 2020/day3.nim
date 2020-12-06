import sequtils
import strutils
import sugar
import tables
import ./aoc_utils
import unittest

const
  stepx = 3
  stepy = 1

type
  Slope = seq[string]
  SlopeState = object
    slope: Slope
    posx: int
    posy: int
    numHits: int

proc newSlopeState(slope: Slope): SlopeState =
  SlopeState(slope: slope, posx: 0, posy: 0, numHits: 0)

proc extendSlope(slope: Slope): Slope =
  ## Extend the slope to the right by copying the existing scope once.
  result = newSeq[string]()
  for row in slope:
    result.add(row & row)

proc step(state: var SlopeState): bool =
  ## Take a single step down the slope,
  ## - incrementing the current position,
  ## - marking if a tree or open snow was hit,
  ## - and incrementing the count of number of trees hit.
  let
    newx = state.posx + stepx
    newy = state.posy + stepy
  # Check if the new x position is in bounds. If not, extend the slope to the
  # side.
  if newx >= state.slope[0].len:
    state.slope = extendSlope(state.slope)
  # Check if the new y position is in bounds. If not, we are done.
  if newy >= state.slope.len:
    return true
  case state.slope[newy][newx]
    of '#':
      state.numHits += 1
      # state.slope[newx][newy] = 'X'
    of '.':
      # state.slope[newx][newy] = 'O'
      discard
    else:
      raise
  state.posx = newx
  state.posy = newy
  false

proc run(state: var SlopeState): int =
  ## Take single steps down the slope until the bottom is reached, returning
  ## how many trees have been hit.
  var atBottom = false
  while not atBottom:
    atBottom = state.step()
  state.numHits

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
  var state = newSlopeState(example.splitlines())
  check: state.run() == 7

when isMainModule:
  let
    inputLines = readAllLines("day3_input.txt")
  var state = newSlopeState(inputLines)
  echo "part 1: ", state.run()
