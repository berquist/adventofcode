import math
import sequtils
import strutils
import tables
import ./aoc_utils
import unittest

func parseSchedule(schedule: string): seq[int] =
  schedule.split(",").filterIt(it != "x").mapIt(it.parseInt)

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

suite "day13":
  test "part 1":
    let
      time = 939
      schedule = @[7, 13, 59, 31, 19]
    check: "7,13,x,x,59,x,31,19".parseSchedule == schedule
    check: schedule.findClosest(time) == 295

when isMainModule:
  let input = "day13_input.txt".readAllLines
  assert input.len == 2
  let
    time = input[0].parseInt
    schedule = input[1].parseSchedule
  echo "part 1: ", schedule.findClosest(time)
