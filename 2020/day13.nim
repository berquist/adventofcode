import math
import sequtils
import strutils
import tables
import ./aoc_utils
import unittest

const freeSpot = -1

func parseSchedule1(schedule: string): seq[int] =
  schedule.split(",").filterIt(it != "x").mapIt(it.parseInt)

func parseSchedule2(schedule: string): seq[int] =
  schedule.split(",").mapIt(if it == "x": freeSpot else: it.parseInt)

func findClosest(schedule: seq[int]; time: int): int =
  var timeToRoute: Table[int, int]
  for route in schedule:
    let
      lo = time - time.floorMod(route)
      hi = lo + route
    timeToRoute[hi - time] = route
  var shortestTime = 99999
  for time in timeToRoute.keys:
    if time < shortestTime:
      shortestTime = time
  shortestTime * timeToRoute[shortestTime]

proc part2[T: SomeInteger](schedule: seq[T]): T =
  var
    offsetToRoute: Table[T, T]
  for offset, route in schedule:
    if route != freeSpot:
      offsetToRoute[offset] = route
  let stepDistance = offsetToRoute[0]
  for i in countup(0, T.high, stepDistance):
    var mods: seq[T]
    for offset, route in offsetToRoute.pairs:
      mods.add((i + offset) mod route)
    if mods.foldl(a + b) == 0:
      return i

suite "day13":
  test "part 1":
    let
      time = 939
      schedule = @[7, 13, 59, 31, 19]
    check: "7,13,x,x,59,x,31,19".parseSchedule1 == schedule
    check: schedule.findClosest(time) == 295
  test "part 2":
    let schedule = @[7, 13, freeSpot, freeSpot, 59, freeSpot, 31, 19]
    check: "7,13,x,x,59,x,31,19".parseSchedule2 == schedule
    check: @[17, freeSpot, 13, 19].part2 == 3417
    check: schedule.part2 == 1068781
    check: @[67, 7, 59, 61].part2 == 754018
    check: @[67, freeSpot, 7, 59, 61].part2 == 779210
    check: @[67, 7, freeSpot, 59, 61].part2 == 1261476
    check: @[1789, 37, 47, 1889].part2 == 1202161486

when isMainModule:
  let input = "day13_input.txt".readAllLines
  assert input.len == 2
  let
    time = input[0].parseInt
    schedule1 = input[1].parseSchedule1
    schedule2 = input[1].parseSchedule2
  echo "part 1: ", schedule1.findClosest(time)
  echo "part 2: ", schedule2.part2
