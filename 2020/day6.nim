import sets
import sequtils
import strutils
import sugar
import unittest

func getGroups(input: string): seq[string] =
  input.split("\n\n")

func clean(s: string): string =
  s.strip.replace("\n")

func countUniqueAnswers(group: string): int =
  group.clean.toHashSet.len

func countUniqueAnswersAcrossGroups(groups: seq[string]): int =
  groups.mapIt(it.countUniqueAnswers).foldl(a + b)

proc countAnswerIntersection(groups: seq[string]): int =
  # Within each group, split to get a person's answers. Convert these answers
  # to a set, then take the set intersection within a group.
  groups.map(g => g.split.map(p => p.toHashSet).filterIt(it.len > 0).foldl(a * b).len).foldl(a + b)

suite "day6":
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
    example3 = """
cf
cf
fc

rvfjwkpmyxaunl
knarjfvwpmuxyl
vjapkmrlufynxw
yplafmknjvrwux

vguhiezflwxspkbtqmoyd
puteyokqzbigsxwvfmdlh
khuwgdieqlxsfbotzvmpy

xji
yjx
yxj
"""
  test "part 1":
    check: example2.countUniqueAnswers == 6
    check: example2.getGroups.countUniqueAnswersAcrossGroups == 6
    check: example1.getGroups.countUniqueAnswersAcrossGroups == 11
  test "part 2":
    check: example1.getGroups.countAnswerIntersection == 6
    check: example2.getGroups.countAnswerIntersection == 3
    check: example3.getGroups.countAnswerIntersection == (2 + 14 + 21 + 2)

when isMainModule:
  let groups = "day6_input.txt".readFile.getGroups
  echo "part 1: ", groups.countUniqueAnswersAcrossGroups
  echo "part 2: ", groups.countAnswerIntersection
