import options
import sequtils
import sets
import strutils
import ./aoc_utils
import unittest

func isValid[T: SomeNumber](nums: openArray[T], requestedSum: T): bool =
  ## Is the give number a sum of two numbers in the given sequence?
  var candidatePartner: T
  for num in nums:
    candidatePartner = requestedSum - num
    if candidatePartner in nums and num != candidatePartner:
      return true

func isValidTape[T: SomeNumber](nums: openArray[T], windowSize: int): Option[T] =
  var window: seq[T]
  for i in low(nums) + windowSize ..< nums.len:
    window = nums[i - windowSize ..< i]
    if window.len > 0:
      if not window.isValid(nums[i]):
        return some(nums[i])

func findEncryptionWeakness[T: SomeNumber](nums: openArray[T], targetNum: T): Option[T] =
  for start in low(nums) ..< high(nums):
    if nums[start] <= targetNum:
      var sum = 0
      for fin in start + 1 .. high(nums):
        if sum < targetNum:
          sum += nums[fin]
        if sum > targetNum:
          break
        if sum == targetNum:
          let window = nums[start..fin]
          return some(window.min + window.max)

suite "day9":
  let larger = [35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102,
                117, 150, 182, 127, 219, 299, 277, 309, 576]
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
    check: ex2.isValidTape(windowSize) == none(int)
  test "part 1 ex 2":
    check: larger.isValidTape(5) == some(127)
  test "part 2":
    check: larger.findEncryptionWeakness(127) == some(62)

when isMainModule:
  let
    input = "day9_input.txt".readAllLines
    inputInts = input.mapIt(it.parseInt)
    nosum = inputInts.isValidTape(25).get
  echo "part 1: ", nosum
  echo "part 2: ", inputInts.findEncryptionWeakness(nosum).get
