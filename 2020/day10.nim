import algorithm
import sequtils
import sets
import strutils
import tables
import unittest
import ./aoc_utils

# func makeCandidates(joltage: int, minDiff: int = 1, maxDiff: int = 3): seq[int] =
#   for toDiff in minDiff..maxDiff:
#     let candidate = joltage - toDiff
#     if candidate >= 0:
#       result.add candidate

func makeCandidates(joltage: int, minDiff: int = 1, maxDiff: int = 3): seq[int] =
  for diff in minDiff..maxDiff:
    result.add joltage + diff

func getDifferences(adapters: seq[int]): Table[int, int] =
  let
    # reversedAdapterJoltages = adapters.sorted(Descending)
    # allJoltages = @[reversedAdapterJoltages[0] + 3] & reversedAdapterJoltages & @[0]
    adapterJoltages = adapters.sorted
    orderedJoltages = @[0] & adapterJoltages & @[adapterJoltages[high(adapterJoltages)] + 3]
    allJoltages = orderedJoltages.toHashSet
  # Because you have to use the closest values, you have to go in order, so
  # can't use a set.
  #
  # var
  #   allJoltages = adapters.toHashSet
  #   candidates: seq[int]
  # allJoltages.incl(adapters.foldl(max(a, b)) + 3)
  # allJoltages.incl 0
  # for joltage in allJoltages:
  #   candidates = joltage.makeCandidates
  var
    candidates: seq[int]
    diff: int
  for joltage in orderedJoltages:
    candidates = joltage.makeCandidates
    for candidate in candidates:
      if candidate in allJoltages:
        diff = candidate - joltage
        if diff notin result:
          result[diff] = 1
        else:
          result[diff] += 1
        break

func getResult(adapters: seq[int]): int =
  let diffs = adapters.getDifferences
  diffs[1] * diffs[3]

suite "day10":
  let
    adapters1 = @[16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4]
    adapters2 = @[28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45,
                  19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3]
  test "part 1":
    check: adapters1.getDifferences == {1: 7, 3: 5}.toTable
    check: adapters2.getDifferences == {1: 22, 3: 10}.toTable
    check: adapters1.getResult == 35
    check: adapters2.getResult == 220

when isMainModule:
  let input = "day10_input.txt".readAllLines.mapIt(it.parseInt)
  echo "part 1: ", input.getResult
