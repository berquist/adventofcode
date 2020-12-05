import sequtils
import strutils
import sugar
import tables
import ./aoc_utils
import unittest

type
  Candidate = object
    password: string
    reqCharacter: char
    minReqCharacter: int
    maxReqCharacter: int

func parseCandidate(s: string): Candidate =
  let
    tokens = s.split()
    rangeReqCharacter = tokens[0].split('-').map(t => parseInt(t))
  Candidate(
    password: tokens[2],
    reqCharacter: tokens[1][0],
    minReqCharacter: rangeReqCharacter[0],
    maxReqCharacter: rangeReqCharacter[1]
  )

proc isValidPassword(candidate: Candidate): bool =
  let
    frequencies = toCountTable(candidate.password)
    magic = -1
    count = frequencies.getOrDefault(candidate.reqCharacter, magic)
  if count == magic:
    return false
  count in candidate.minReqCharacter..candidate.maxReqCharacter

suite "day2":
  test "passwordValidation":
    let
      example = """1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc"""
      exampleLines = example.splitlines()
      candidates = exampleLines.map(s => parseCandidate(s))
    check: candidates[0].isValidPassword
    check: not candidates[1].isValidPassword
    check: candidates[2].isValidPassword

when isMainModule:
  let
    inputLines = readAllLines("day2_input.txt")
    candidates = inputLines.map(l => parseCandidate(l))
  echo "part1: ", candidates.map(c => isValidPassword(c)).count(true)
