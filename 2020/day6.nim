import sets
import sequtils
import strutils
import unittest

func getGroups(input: string): seq[string] =
  input.split("\n\n")

func countUniqueAnswers(group: string): int =
  var uniqueAnswers = group.toHashSet
  uniqueAnswers.excl('\n')
  uniqueAnswers.len

func countUniqueAnswersAcrossGroups(groups: seq[string]): int =
  groups.mapIt(it.countUniqueAnswers).foldl(a + b)

suite "day6":
  test "part 1":
    let
      example1 = """abc

a
b
c

ab
ac

a
a
a
a

b"""
      example2 = """abcx
abcy
abcz"""
    check: example2.countUniqueAnswers == 6
    check: example2.getGroups.countUniqueAnswersAcrossGroups == 6
    check: example1.getGroups.countUniqueAnswersAcrossGroups == 11

when isMainModule:
  let input = "day6_input.txt".readFile
  echo "part 1: ", input.getGroups.countUniqueAnswersAcrossGroups
