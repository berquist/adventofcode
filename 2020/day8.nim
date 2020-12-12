import sequtils
import sets
import strutils
import ./aoc_utils
import tables
import unittest

type
  Op = enum
    nop
    acc
    jmp
  Instruction = object
    op: Op
    arg: int
  GameConsole = object
    accumulator: int
    tape: seq[Instruction]
    tapePos: int
    seenPos: HashSet[int]
    hitLoop: bool
    booted: bool

const
  opMap = {"nop": Op.nop, "acc": Op.acc, "jmp": Op.jmp}.toTable
  opsToFlip = [Op.nop, Op.jmp].toHashSet

func lineToInstruction(line: string): Instruction =
  let tokens = line.strip.split
  assert tokens.len == 2
  result.op = opMap[tokens[0]]
  result.arg = tokens[1].parseInt

func initConsole(tape: seq[Instruction]): GameConsole =
  result.tape = tape

proc runInstruction(console: GameConsole): GameConsole =
  # short-circuit
  if console.booted:
    return console
  result = console.deepCopy
  let instruction = result.tape[result.tapePos]
  if result.tapePos in console.seenPos:
    result.hitLoop = true
    return result
  else:
    result.seenPos.incl result.tapePos
  case instruction.op
  of nop:
    result.tapePos += 1
  of acc:
    result.accumulator += instruction.arg
    result.tapePos += 1
  of jmp:
    result.tapePos += instruction.arg
  if result.tapePos == result.tape.len:
    result.booted = true

proc runInstructionsToLoop(console: GameConsole): GameConsole =
  result = console.deepCopy
  while not (result.hitLoop or result.booted):
    result = result.runInstruction

func flipOpBetweenNopAndJmp(op: Op): Op =
  case op
  of Op.nop: Op.jmp
  of Op.acc: Op.acc
  of Op.jmp: Op.nop

func flipOpBetweenNopAndJmp(inst: Instruction): Instruction =
  result = inst.deepCopy
  result.op = result.op.flipOpBetweenNopAndJmp

proc findBootingConsole(tape: seq[Instruction]): GameConsole =
  for i in low(tape)..high(tape):
    if tape[i].op in opsToFlip:
      var singlyFlippedTape = tape.deepCopy
      singlyFlippedTape[i] = singlyFlippedTape[i].flipOpBetweenNopAndJmp
      let console = singlyFlippedTape.initConsole.runInstructionsToLoop
      if console.booted:
        return console

suite "day8":
  let
    example = """
nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6
""".strip.splitlines.mapIt(it.lineToInstruction)
  test "part 1":
      # initialize
      let console1 = example.initConsole
      check: console1.accumulator == 0
      check: console1.tapePos == 0
      check: console1.seenPos == initHashSet[int]()
      # nop +0
      let console2 = console1.runInstruction()
      check: console2.accumulator == 0
      check: console2.tapePos == 1
      check: console2.seenPos == [0].toHashSet
      # acc +1
      let console3 = console2.runInstruction()
      check: console3.accumulator == 1
      check: console3.tapePos == 2
      check: console3.seenPos == [0, 1].toHashSet
      # jmp +4
      let console4 = console3.runInstruction()
      check: console4.accumulator == 1
      check: console4.tapePos == 6
      check: console4.seenPos == [0, 1, 2].toHashSet
      # acc +1
      let console5 = console4.runInstruction()
      check: console5.accumulator == 2
      check: console5.tapePos == 7
      check: console5.seenPos == [0, 1, 2, 6].toHashSet
      # jmp -4
      let console6 = console5.runInstruction()
      check: console6.accumulator == 2
      check: console6.tapePos == 3
      check: console6.seenPos == [0, 1, 2, 6, 7].toHashSet
      # acc +3
      let console7 = console6.runInstruction()
      check: console7.accumulator == 5
      check: console7.tapePos == 4
      check: console7.seenPos == [0, 1, 2, 6, 7, 3].toHashSet
      # jmp -3
      let console8 = console7.runInstruction()
      check: console8.accumulator == 5
      check: console8.tapePos == 1
      check: console8.seenPos == [0, 1, 2, 6, 7, 3, 4].toHashSet
      check: not console8.hitLoop
      # acc +1 (stop here)
      let console9 = console8.runInstruction()
      check: console9.accumulator == 5
      check: console9.tapePos == 1
      check: console9.seenPos == [0, 1, 2, 6, 7, 3, 4].toHashSet
      check: console9.hitLoop
      let console10 = example.initConsole.runInstructionsToLoop
      check: console10.accumulator == 5
  test "part 2":
    let
      terminatingExample = """
nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
nop -4
acc +6
""".strip.splitlines.mapIt(it.lineToInstruction)
      # initialize
    let console1 = terminatingExample.initConsole
    check: console1.accumulator == 0
    check: console1.tapePos == 0
    check: console1.seenPos == initHashSet[int]()
    # nop +0
    let console2 = console1.runInstruction()
    check: console2.accumulator == 0
    check: console2.tapePos == 1
    check: console2.seenPos == [0].toHashSet
    # acc +1
    let console3 = console2.runInstruction()
    check: console3.accumulator == 1
    check: console3.tapePos == 2
    check: console3.seenPos == [0, 1].toHashSet
    # jmp +4
    let console4 = console3.runInstruction()
    check: console4.accumulator == 1
    check: console4.tapePos == 6
    check: console4.seenPos == [0, 1, 2].toHashSet
    # acc +1
    let console5 = console4.runInstruction()
    check: console5.accumulator == 2
    check: console5.tapePos == 7
    check: console5.seenPos == [0, 1, 2, 6].toHashSet
    # *** nop -4
    let console6 = console5.runInstruction()
    check: console6.accumulator == 2
    check: console6.tapePos == 8
    check: console6.seenPos == [0, 1, 2, 6, 7].toHashSet
    # *** acc +6 and booted
    let console7 = console6.runInstruction()
    check: console7.accumulator == 8
    check: console7.tapePos == 9
    check: console7.seenPos == [0, 1, 2, 6, 7, 8].toHashSet
    check: console7.booted
    # let console8 = terminatingExample.findBootingConsole
    # check: console8

when isMainModule:
  let input = "day8_input.txt".readAllLines.mapIt(it.lineToInstruction)
  echo "part 1: ", input.initConsole.runInstructionsToLoop.accumulator
  echo "part 2: ", input.findBootingConsole.accumulator
