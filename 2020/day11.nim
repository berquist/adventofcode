import sequtils
import strutils
import unittest
import ./aoc_utils

func inGrid(point: (int, int); lenx, leny: int): bool =
  (0 <= point[0]) and (point[0] < leny) and (0 <= point[1]) and (point[1] < lenx)

proc makePass(grid: seq[string]): seq[string] =
  result = grid.deepcopy
  let
    nrows = result.len
    ncols = result[0].len
  for i in 0 ..< nrows:
    for j in 0 ..< ncols:
      # Consider the box of 8 points surrounding each point (yes, including
      # the diagonal corners).  Apply the rules simultaneously: derive them
      # from the unmodified/original grid.
      #
      # - A floor point (.) never changes.
      #
      # - If a point is empty (L) and there are *no* occupied points (#)
      #   adjacent, the point becomes occupied.
      #
      # - If a point is occupied (#) and *four or more* adjacent points are
      #   occupied, the point becomes empty (L).
      #
      # - Otherwise, a point's state doesn't change.
      let
        nw = (i - 1, j - 1)
        up = (i - 1, j)
        ne = (i - 1, j + 1)
        right = (i, j + 1)
        se = (i + 1, j - 1)
        down = (i + 1, j)
        sw = (i + 1, j + 1)
        left = (i, j - 1)
      var points: seq[char]
      if nw.inGrid(ncols, nrows):
        points.add(grid[nw[0]][nw[1]])
      if up.inGrid(ncols, nrows):
        points.add(grid[up[0]][up[1]])
      if ne.inGrid(ncols, nrows):
        points.add(grid[ne[0]][ne[1]])
      if right.inGrid(ncols, nrows):
        points.add(grid[right[0]][right[1]])
      if se.inGrid(ncols, nrows):
        points.add(grid[se[0]][se[1]])
      if down.inGrid(ncols, nrows):
        points.add(grid[down[0]][down[1]])
      if sw.inGrid(ncols, nrows):
        points.add(grid[sw[0]][sw[1]])
      if left.inGrid(ncols, nrows):
        points.add(grid[left[0]][left[1]])
      let numOcc = points.count('#')
      var newState = grid[i][j]
      if newState == 'L' and numOcc == 0:
        newState = '#'
      elif newState == '#' and numOcc >= 4:
        newState = 'L'
      result[i][j] = newState

proc iteratePoints(start: seq[string]): seq[string] =
  var
    prev = start
    curr = start.makePass
  while true:
    if curr == prev:
      return curr
    prev = curr
    curr = curr.makePass

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
    let pass1 = init.splitlines.makePass.join("\n")
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
    let pass2 = pass1.splitlines.makePass.join("\n")
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
    let pass3 = pass2.splitlines.makePass.join("\n")
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
    let pass4 = pass3.splitlines.makePass.join("\n")
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
    let pass5 = pass4.splitlines.makePass.join("\n")
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
    let pass6 = pass5.splitlines.makePass.join("\n")
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
    let selfConsistent = init.splitlines.iteratePoints
    check: selfConsistent.join("\n") == pass6
    check: selfConsistent.countOccupied == 37

when isMainModule:
  let input = "day11_input.txt".readAllLines
  echo "part 1: ", input.iteratePoints.countOccupied
