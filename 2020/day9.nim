import sequtils
import sets
import strutils
import ./aoc_utils
import unittest


proc isValid[T: SomeNumber](nums: openArray[T], requestedSum: T): bool =
  ## Is the give number a sum of two numbers in the given sequence?
  var candidatePartner: T
  for num in nums:
    candidatePartner = requestedSum - num
    if candidatePartner in nums and num != candidatePartner:
      return true

# proc isValid[T: SomeNumber](nums: openArray[T], requestedSum: T, windowSize: int): bool =
#   nums[nums.len - windowSize ..< nums.len].isValid(requestedSum)

proc isValidTape[T: SomeNumber](nums: openArray[T], windowSize: int): HashSet[T] =
  var window: seq[T]
  for i in low(nums) + windowSize ..< nums.len:
    window = nums[i - windowSize ..< i]
    if window.len > 0:
      if not window.isValid(nums[i]):
        result.incl nums[i]
  

suite "day9":
  test "part 1 ex 1":
    let ex1 = (1..25).toSeq
    check: ex1.isValid(26)
    check: ex1.isValid(49)
    check: not ex1.isValid(100)
    check: not ex1.isValid(50)
    let ex2_0 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,
                 15, 16, 17, 18, 19, 21, 22, 23, 24, 25, 45]
    check: ex2_0.isValid(26)
    check: not ex2_0.isValid(65)
    check: ex2_0.isValid(64)
    check: ex2_0.isValid(66)
    let
      ex2 = [20, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,
             15, 16, 17, 18, 19, 21, 22, 23, 24, 25, 45]
      windowSize = 25
    # check: ex2.len == windowSize + 1
    # check: ex2.isValid(26, windowSize)
    # check: not ex2.isValid(65, windowSize)
    # check: ex2.isValid(64, windowSize)
    # check: ex2.isValid(66, windowSize)
    check: ex2.isValidTape(windowSize) == initHashSet[int]()
  test "part 1 ex 2":
    let larger = [35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102,
                  117, 150, 182, 127, 219, 299, 277, 309, 576]
    check: larger.isValidTape(5) == [127].toHashSet

when isMainModule:
  let input = "day9_input.txt".readAllLines
  echo "part 1: ", input.mapIt(it.parseInt).isValidTape(25)
