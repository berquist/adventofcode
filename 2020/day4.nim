import strutils
import unittest

const required_fields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

func getEntries(input: string): seq[string] =
  input.split("\n\n")

func isValid(entry: string): bool =
  for field in required_fields:
    if not entry.contains(field):
      return false
  true

func numValidEntries(input: string): int =
  for entry in input.getEntries:
    result += entry.isValid.int

suite "day4":
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
  check: entries[0].isValid == true
  check: entries[1].isValid == false
  check: entries[2].isValid == true
  check: entries[3].isValid == false
  check: example.numValidEntries == 2

when isMainModule:
  let input = "day4_input.txt".readFile
  echo "part 1: ", input.numValidEntries
