import sequtils
import strutils
import sugar
import tables
import ./aoc_utils
import unittest

type
  Candidate = object
    password: string
    ch: char
    n1: int
    n2: int

func parseCandidate(s: string): Candidate =
  let
    tokens = s.split()
    rangeCh = tokens[0].split('-').map(t => parseInt(t))
  Candidate(
    password: tokens[2],
    ch: tokens[1][0],
    n1: rangeCh[0],
    n2: rangeCh[1]
  )

func isValidPassword1(candidate: Candidate): bool =
  let
    frequencies = toCountTable(candidate.password)
    magic = -1
    count = frequencies.getOrDefault(candidate.ch, magic)
  if count == magic:
    return false
  count in candidate.n1..candidate.n2

proc isValidPassword2(candidate: Candidate): bool =
  let
    creq = candidate.ch
    has1 = candidate.password[candidate.n1 - 1] == creq
    has2 = candidate.password[candidate.n2 - 1] == creq
  has1 xor has2

suite "day2":
  let
    example = """1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc"""
    exampleLines = example.splitlines()
    candidates = exampleLines.map(s => parseCandidate(s))
  test "passwordValidation1":
    check: candidates[0].isValidPassword1
    check: not candidates[1].isValidPassword1
    check: candidates[2].isValidPassword1
  test "passwordValidation2":
    check: candidates[0].isValidPassword2
    check: not candidates[1].isValidPassword2
    check: not candidates[2].isValidPassword2

when isMainModule:
  let
    inputLines = readAllLines("day2_input.txt")
    candidates = inputLines.map(l => parseCandidate(l))
  echo "part1: ", candidates.map(c => isValidPassword1(c)).count(true)
  echo "part2: ", candidates.map(c => isValidPassword2(c)).count(true)
