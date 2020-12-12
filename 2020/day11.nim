import sequtils
import strutils
import unittest
import ./aoc_utils

type
  Point = tuple
    x: int
    y: int

func `+`(p1, p2: Point): Point =
  (p1[0] + p2[0], p1[1] + p2[1])

func `+=`(left: var Point, right: Point) =
  left[0] += right[0]
  left[1] += right[1]

const directions = [
  (-1, -1),
  (-1, 0),
  (-1, 1),
  (0, 1),
  (1, -1),
  (1, 0),
  (1, 1),
  (0, -1)
]

func inGrid(point: (int, int); lenx, leny: int): bool =
  (0 <= point[0]) and (point[0] < leny) and (0 <= point[1]) and (point[1] < lenx)

proc makePass(grid: seq[string], minFlipState: int, beam: bool): seq[string] =
  result = grid.deepcopy
  let
    nrows = result.len
    ncols = result[0].len
  for i in 0 ..< nrows:
    for j in 0 ..< ncols:
      var points: seq[char]
      for direction in directions:
        # The "beam" approach for part 2 was adapted from
        # https://github.com/Abigail/AdventOfCode2020/blob/f3b5a3619e77f3a6dfca957349695bf7e14a46d4/Day_11/solution.pl
        var newPoint = (i, j) + direction
        if beam:
          while newPoint.inGrid(ncols, nrows) and grid[newPoint[0]][newPoint[1]] == '.':
            newPoint += direction
        if newPoint.inGrid(ncols, nrows):
          points.add grid[newPoint[0]][newPoint[1]]
      let numOcc = points.count('#')
      var newState = grid[i][j]
      if newState == 'L' and numOcc == 0:
        newState = '#'
      elif newState == '#' and numOcc >= minFlipState:
        newState = 'L'
      result[i][j] = newState

proc iteratePoints(start: seq[string], minFlipState: int, beam: bool): seq[string] =
  var
    prev = start
    curr = start.makePass(minFlipState, beam)
  while true:
    if curr == prev:
      return curr
    prev = curr
    curr = curr.makePass(minFlipState, beam)

func countOccupied(grid: seq[string]): int =
  grid.join("\n").count("#")

suite "day11":
  let
    init = """
L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"""
  test "part 1":
    let pass1 = init.splitlines.makePass(4, false).join("\n")
    check: pass1 == """
#.##.##.##
#######.##
#.#.#..#..
####.##.##
#.##.##.##
#.#####.##
..#.#.....
##########
#.######.#
#.#####.##"""
    let pass2 = pass1.splitlines.makePass(4, false).join("\n")
    check: pass2 == """
#.LL.L#.##
#LLLLLL.L#
L.L.L..L..
#LLL.LL.L#
#.LL.LL.LL
#.LLLL#.##
..L.L.....
#LLLLLLLL#
#.LLLLLL.L
#.#LLLL.##"""
    let pass3 = pass2.splitlines.makePass(4, false).join("\n")
    check: pass3 == """
#.##.L#.##
#L###LL.L#
L.#.#..#..
#L##.##.L#
#.##.LL.LL
#.###L#.##
..#.#.....
#L######L#
#.LL###L.L
#.#L###.##"""
    let pass4 = pass3.splitlines.makePass(4, false).join("\n")
    check: pass4 == """
#.#L.L#.##
#LLL#LL.L#
L.L.L..#..
#LLL.##.L#
#.LL.LL.LL
#.LL#L#.##
..L.L.....
#L#LLLL#L#
#.LLLLLL.L
#.#L#L#.##"""
    let pass5 = pass4.splitlines.makePass(4, false).join("\n")
    check: pass5 == """
#.#L.L#.##
#LLL#LL.L#
L.#.L..#..
#L##.##.L#
#.#L.LL.LL
#.#L#L#.##
..L.L.....
#L#L##L#L#
#.LLLLLL.L
#.#L#L#.##"""
    let pass6 = pass5.splitlines.makePass(4, false).join("\n")
    check: pass6 == """
#.#L.L#.##
#LLL#LL.L#
L.#.L..#..
#L##.##.L#
#.#L.LL.LL
#.#L#L#.##
..L.L.....
#L#L##L#L#
#.LLLLLL.L
#.#L#L#.##"""
    let selfConsistent = init.splitlines.iteratePoints(4, false)
    check: selfConsistent.join("\n") == pass6
    check: selfConsistent.countOccupied == 37
  test "part 2":
    let pass1 = init.splitlines.makePass(5, true).join("\n")
    check: pass1 == """
#.##.##.##
#######.##
#.#.#..#..
####.##.##
#.##.##.##
#.#####.##
..#.#.....
##########
#.######.#
#.#####.##"""
    let pass2 = pass1.splitlines.makePass(5, true).join("\n")
    check: pass2 == """
#.LL.LL.L#
#LLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLL#
#.LLLLLL.L
#.LLLLL.L#"""
    let pass3 = pass2.splitlines.makePass(5, true).join("\n")
    check: pass3 == """
#.L#.##.L#
#L#####.LL
L.#.#..#..
##L#.##.##
#.##.#L.##
#.#####.#L
..#.#.....
LLL####LL#
#.L#####.L
#.L####.L#"""
    let pass4 = pass3.splitlines.makePass(5, true).join("\n")
    check: pass4 == """
#.L#.L#.L#
#LLLLLL.LL
L.L.L..#..
##LL.LL.L#
L.LL.LL.L#
#.LLLLL.LL
..L.L.....
LLLLLLLLL#
#.LLLLL#.L
#.L#LL#.L#"""
    let pass5 = pass4.splitlines.makePass(5, true).join("\n")
    check: pass5 == """
#.L#.L#.L#
#LLLLLL.LL
L.L.L..#..
##L#.#L.L#
L.L#.#L.L#
#.L####.LL
..#.#.....
LLL###LLL#
#.LLLLL#.L
#.L#LL#.L#"""
    let pass6 = pass5.splitlines.makePass(5, true).join("\n")
    check: pass6 == """
#.L#.L#.L#
#LLLLLL.LL
L.L.L..#..
##L#.#L.L#
L.L#.LL.L#
#.LLLL#.LL
..#.L.....
LLL###LLL#
#.LLLLL#.L
#.L#LL#.L#"""

when isMainModule:
  let input = "day11_input.txt".readAllLines
  echo "part 1: ", input.iteratePoints(4, false).countOccupied
  echo "part 2: ", input.iteratePoints(5, true).countOccupied
