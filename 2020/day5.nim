import math
import sequtils
import ./aoc_utils
import unittest

proc partition(binaryString: string, lowerChar, upperChar: char): int =
  var
    ilower = 0
    iupper = (2 ^ binaryString.len) - 1
    ranges = newSeq[(int, int)]()
  for c in binaryString:
    if c == lowerChar:
      iupper = iupper - ((iupper - ilower) div 2) - 1
    else:
      ilower = ilower + ((iupper - ilower) div 2) + 1
    ranges.add((ilower, iupper))
  assert ilower == iupper
  ilower

proc getRow(pass: string): int =
  pass[..6].partition('F', 'B')

proc getCol(pass: string): int =
  pass[7..9].partition('L', 'R')

proc getSeatId(pass: string): int =
  pass.getRow * 8 + pass.getCol

suite "day5":
  test "part 1":
    let ex1 = "FBFBBFFRLR"
    check: ex1.getRow == 44
    check: ex1.getCol == 5
    check: ex1.getSeatId == 357
    let ex2 = "BFFFBBFRRR"
    check: ex2.getRow == 70
    check: ex2.getCol == 7
    check: ex2.getSeatId == 567
    let ex3 = "FFFBBBFRRR"
    check: ex3.getRow == 14
    check: ex3.getCol == 7
    check: ex3.getSeatId == 119
    let ex4 = "BBFFBBFRLL"
    check: ex4.getRow == 102
    check: ex4.getCol == 4
    check: ex4.getSeatId == 820

when isMainModule:
  let input = "day5_input.txt".readAllLines
  echo "part 1: ", input.mapIt(it.getSeatId).foldl(max(a, b))
