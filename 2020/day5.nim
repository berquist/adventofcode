import algorithm
import math
import sequtils
import ./aoc_utils
import unittest

func partition(binaryString: string, lowerChar, upperChar: char): int =
  var
    ilower = 0
    iupper = (2 ^ binaryString.len) - 1
  for c in binaryString:
    if c == lowerChar:
      iupper = iupper - ((iupper - ilower) div 2) - 1
    else:
      ilower = ilower + ((iupper - ilower) div 2) + 1
  assert ilower == iupper
  ilower

func getRow(pass: string): int =
  pass[..6].partition('F', 'B')

func getCol(pass: string): int =
  pass[7..9].partition('L', 'R')

func getSeatId(pass: string): int =
  pass.getRow * 8 + pass.getCol

func groups[T](things: seq[T], count: int): seq[seq[T]] =
  result = newSeq[seq[T]]()
  var
    start = 0
    fin = start + count
  while fin < things.len:
    result.add(things[start..<fin])
    start = fin
    fin += count
  if start < things.len:
    result.add(things[start..<things.len])

func findSeat(seatIds: seq[int]): int =
  let sortedIds = seatIds.sorted
  var prevId = sortedIds[0]
  for currId in sortedIds[1..<sortedIds.len]:
    if currId - prevId == 2:
      return currId - 1
    prevId = currId

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
  test "part 2":
    check @["a", "b", "c", "d"].groups(2) == @[@["a", "b"], @["c", "d"]]
    check @["a", "b", "c", "d", "e"].groups(2) == @[@["a", "b"], @["c", "d"], @["e"]]
    check @["a", "b", "c", "d", "e", "f"].groups(3) == @[@["a", "b", "c"], @["d", "e", "f"]]
    check @["a", "b", "c", "d", "e"].groups(3) == @[@["a", "b", "c"], @["d", "e"]]

when isMainModule:
  let
    input = "day5_input.txt".readAllLines
    seatIds = input.mapIt(it.getSeatId)
  echo "part 1: ", seatIds.foldl(max(a, b))
  echo "part 2: ", seatIds.findSeat
