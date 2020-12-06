import re
import sequtils
import strutils
import sugar
import tables
import unittest

const required_fields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

func getEntries(input: string): seq[string] =
  input.split("\n\n")

func isValid1(entry: string): bool =
  for field in required_fields:
    if not entry.contains(field):
      return false
  true

func numValidEntries1(input: string): int =
  for entry in input.getEntries:
    result += entry.isValid1.int

func isValidByr(inp: string): bool =
  let matches = findAll(inp, re"\d{4}")
  if matches.len != 1:
    return false
  let match = matches[0].parseInt
  match >= 1920 and match <= 2002

func isValidIyr(inp: string): bool =
  let matches = findAll(inp, re"\d{4}")
  if matches.len != 1:
    return false
  let match = matches[0].parseInt
  match >= 2010 and match <= 2020

func isValidEyr(inp: string): bool =
  let matches = findAll(inp, re"\d{4}")
  if matches.len != 1:
    return false
  let match = matches[0].parseInt
  match >= 2020 and match <= 2030

func isValidHgt(inp: string): bool =
  var matches: array[2, string]
  let didMatch = match(inp, re"(\d{2,3})(cm|in)", matches)
  if not didMatch:
    return false
  let height = matches[0].parseInt
  case matches[1]
  of "cm": height >= 150 and height <= 193
  of "in": height >= 59 and height <= 76
  else: raise

func isValidHcl(inp: string): bool =
  let matches = findAll(inp, re"^#[0-9a-f]{6}$")
  matches.len == 1

const validEyeColors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

func isValidEcl(inp: string): bool =
  let matches = findAll(inp, re"\w{3}")
  if matches.len != 1:
    return false
  matches[0] in validEyeColors

func isValidPid(inp: string): bool =
  let matches = findAll(inp, re"^\d{9}$")
  matches.len == 1

const fieldToValidator = {
  "byr": isValidByr,
  "iyr": isValidIyr,
  "eyr": isValidEyr,
  "hgt": isValidHgt,
  "hcl": isValidHcl,
  "ecl": isValidEcl,
  "pid": isValidPid
}.toTable

func isValid2(inp: string): bool =
  let candidatePassportFields = collect(initTable()):
    for part in inp.strip.split:
      let pair = part.strip.split(':')
      {pair[0]: pair[1]}
  for (field, validator) in fieldToValidator.pairs:
    if not candidatePassportFields.contains(field):
      return false
    if not validator(candidatePassportFields[field]):
      return false
  true

proc numValidEntries2(inp: string): int =
  for entry in inp.getEntries:
    result += entry.isValid2.int

suite "day4":
  test "part 1":
    let
      example = """
ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in
""".strip
      entries = example.getEntries
    check: entries.len == 4
    check: entries[0].isValid1 == true
    check: entries[1].isValid1 == false
    check: entries[2].isValid1 == true
    check: entries[3].isValid1 == false
    check: example.numValidEntries1 == 2
  test "part 2":
    check: isValidByr("2002")
    check: not isValidByr("2003")
    check: isValidHgt("60in")
    check: isValidHgt("190cm")
    check: not isValidHgt("190in")
    check: not isValidHgt("190")
    check: isValidHcl("#123abc")
    check: not isValidHcl("#123abz")
    check: not isValidHcl("123abc")
    check: isValidEcl("brn")
    check: not isValidEcl("wat")
    check: isValidPid("000000001")
    check: not isValidPid("0123456789")

    let
      invalidExamples = """
eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007
""".getEntries
      validExamples = """
pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719
""".getEntries
    check: invalidExamples.map(e => e.isValid2) == @[false, false, false, false]
    check: validExamples.map(e => e.isValid2) == @[true, true, true, true]

when isMainModule:
  let input = "day4_input.txt".readFile
  echo "part 1: ", input.numValidEntries1
  echo "part 2: ", input.numValidEntries2
