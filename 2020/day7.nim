import re
import sets
import strutils
import sugar
import tables
import unittest

type
  Rule = object
    count: int
    bagType: string

proc parseRule(line: string): (string, seq[Rule]) =
  let sides = line.split("contain")
  assert sides.len == 2
  let
    left = sides[0].split()[..1].join(" ")
    right = findAll(sides[1], re"(\d+)\s(\w+\s\w+)\sbag")
  var rules = newSeq[Rule]()
  for s in right:
    let
      tokens = s.split
    rules.add(Rule(count: tokens[0].parseInt, bagType: tokens[1..2].join(" ")))
  (left, rules)

proc parseRules(inp: string): Table[string, seq[Rule]] =
  collect(initTable()):
    for line in inp.splitlines:
      let (left, right) = line.parseRule
      {left: right}

proc walkRules(rules: Table[string, seq[Rule]], desiredBagType: string, currentBagType: string, seenBagTypes: HashSet[string] = initHashSet[string]()): HashSet[string] =
  result = initHashSet[string]()
  for rhs in rules[currentBagType]:
    if rhs.bagType == desiredBagType:
      result = result + seenBagTypes
      result.incl(currentBagType)
    else:
      var seenBagTypesPlusCurrent = seenBagTypes
      seenBagTypesPlusCurrent.incl(currentBagType)
      result = result + rules.walkRules(desiredBagType, rhs.bagType, seenBagTypesPlusCurrent)

proc walkRules(rules: Table[string, seq[Rule]], desiredBagType: string): HashSet[string] =
  ## Make the first pass over all the top-level nodes.
  result = initHashSet[string]()
  for walkBagType, rhses in rules.pairs:
    if walkBagType != desiredBagType:
      for rhs in rhses:
        result = result + rules.walkRules(desiredBagType, rhs.bagType, [walkBagType].toHashSet)

suite "day7":
  test "part 1":
    let
      example = """light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags."""
      exampleRules = example.parseRules
    check: exampleRules.len == 9
    check: exampleRules["dotted black"] == newSeq[Rule]()
    check: exampleRules["bright white"] == @[Rule(count: 1, bagType: "shiny gold")]
    check: exampleRules.walkRules("shiny gold", "dotted black") == initHashSet[string]()
    check: exampleRules.walkRules("shiny gold", "faded blue") == initHashSet[string]()
    check: exampleRules.walkRules("shiny gold", "vibrant plum") == initHashSet[string]()
    check: exampleRules.walkRules("shiny gold", "bright white") == ["bright white"].toHashSet
    check: exampleRules.walkRules("shiny gold", "light red") == ["light red", "bright white", "muted yellow"].toHashSet
    check: exampleRules.walkRules("shiny gold", "dark orange") == ["dark orange", "bright white", "muted yellow"].toHashSet
    check: exampleRules.walkRules("shiny gold") == ["light red", "dark orange", "bright white", "muted yellow"].toHashSet

when isMainModule:
  let input = "day7_input.txt".readFile.strip
  echo "part 1: ", input.parseRules.walkRules("shiny gold").len
