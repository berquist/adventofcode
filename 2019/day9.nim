from algorithm import reversed
from sequtils import map
from strutils import split, parseInt
from sugar import `=>`
import timeit
import unittest

proc stringToProgram(s: string): seq[int] =
  s.split(',').map(i => parseInt(i))

type
  Opcode = enum
    add = 1
    multiply = 2
    input = 3
    output = 4
    jumpIfTrue = 5
    jumpIfFalse = 6
    lessThan = 7
    equals = 8
    adjustRelativeBase = 9
    halt = 99
  Mode = enum
    position = 0
    immediate = 1
    relative = 2
  OutputMode {.pure.} = enum
    ret
    halt

## The length of an Opcode accounts for the number of arguments it takes, plus
## one for the opcode itself.
proc len(oc: Opcode): int =
  case oc:
    of add: 4
    of multiply: 4
    of input: 2
    of output: 2
    of jumpIfTrue: 3
    of jumpIfFalse: 3
    of lessThan: 4
    of equals: 4
    of adjustRelativeBase: 2
    of halt: 1

proc parseInstruction(instruction: string): (seq[Mode], Opcode) =
  assert instruction.len >= 1
  let
    maxNumArguments = 3
    h = high(instruction)
    opcodeStr = if instruction.len == 2: instruction[(h - 1)..h]
                else: $instruction[h]
    opcode = Opcode(parseInt(opcodeStr))
    # The given string can be anywhere between 1-5 digits long.
    modeStr = if instruction.len > 1: instruction[low(instruction)..(h - 2)]
              else: ""
  var splitModes = reversed(modeStr)
  while splitModes.len < maxNumArguments:
    splitModes.add('0')
  result = (splitModes.map(c => Mode(parseInt($c))), opcode)

proc extendProgram[T: SomeInteger](program: seq[T], newLen: int): seq[T] =
  result = program
  while result.len < newLen:
    result.add(0)

proc getPtr[T: SomeInteger](tape: seq[T], offset: T, mode: Mode,
    relativeBase: T = 0): T =
  case mode:
    of Mode.position: tape[offset]
    of Mode.immediate: offset
    of Mode.relative: tape[offset] + relativeBase

proc getArg[T: SomeInteger](tape: seq[T], offset: T, mode: Mode,
    relativeBase: T = 0): T =
  let programPtr = getPtr(tape, offset, mode, relativeBase)
  tape[programPtr]

type
  IntcodeComputer = object
    program: seq[int]
    instructionPointer: int
    relativeBase: int
    output: int
    returned: bool
    halted: bool

template extendProgram(mode: Mode): untyped =
  let
    pos = opcode.len - 1
    actualPtr = getPtr(result.program, result.instructionPointer + pos,
                       mode, result.relativeBase)
  if actualPtr > high(result.program):
    result.program = extendProgram(result.program, 2 * actualPtr)
template first(): untyped =
  extendProgram(modes[0])
  getArg(result.program, result.instructionPointer + 1, modes[0],
         result.relativeBase)
template second(): untyped =
  extendProgram(modes[1])
  getArg(result.program, result.instructionPointer + 2, modes[1],
         result.relativeBase)
template res(): untyped =
  let
    pos = opcode.len - 1
    mode = modes[pos - 1]
  extendProgram(mode)
  let actualPtr = getPtr(result.program, result.instructionPointer + pos,
                         mode, result.relativeBase)
  result.program[actualPtr]

proc processProgram[T: SomeInteger](computer: IntcodeComputer, inputs: seq[T],
                                    outputMode: OutputMode = ret): IntcodeComputer =
  result = computer
  result.halted = false
  var inputCounter: T
  while true:
    result = result.step(inputs, outputMode, inputCounter)
    if result.returned:
      break
    else:
      case outputMode:
        of OutputMode.ret:
          if result.output != 0:
            break
        of OutputMode.halt:
          if result.halted:
            break

proc step[T: SomeInteger](computer: IntcodeComputer, inputs: seq[T],
                          outputMode: OutputMode, inputCounter: var T): IntcodeComputer =
  result = computer
  result.returned = false
  var
    modes: seq[Mode]
    opcode: Opcode
  try:
    (modes, opcode) = parseInstruction($result.program[
        result.instructionPointer])
  except RangeError:
    return result
  except IndexError:
    return result
  case opcode:
    of Opcode.add:
      res() = first() + second()
      result.instructionPointer += opcode.len
    of Opcode.multiply:
      res() = first() * second()
      result.instructionPointer += opcode.len
    of Opcode.input:
      res() = inputs[inputCounter]
      inputCounter += 1
      result.instructionPointer += opcode.len
    of Opcode.output:
      result.output = first()
      result.instructionPointer += opcode.len
      case outputMode:
        of OutputMode.ret:
          if result.output != 0:
            result.returned = true
            return result
        of OutputMode.halt:
          result.returned = true
          return result
    of Opcode.jumpIfTrue:
      result.instructionPointer = if first() != 0: second()
                                  else: result.instructionPointer + opcode.len
    of Opcode.jumpIfFalse:
      result.instructionPointer = if first() == 0: second()
                                  else: result.instructionPointer + opcode.len
    of Opcode.lessThan:
      res() = T(first() < second())
      result.instructionPointer += opcode.len
    of Opcode.equals:
      res() = T(first() == second())
      result.instructionPointer += opcode.len
    of Opcode.adjustRelativeBase:
      result.relativeBase += first()
      result.instructionPointer += opcode.len
    of Opcode.halt:
      result.instructionPointer += opcode.len
      case outputMode:
        of OutputMode.ret:
          return result
        of OutputMode.halt:
          result.halted = true

proc run(unprocessedProgram: seq[int], input: int): int {.discardable.} =
  var computer = IntcodeComputer(program: unprocessedProgram)
  while true:
    computer = computer.processProgram(@[input], OutputMode.halt)
    if computer.halted or computer.returned:
      break
  result = computer.output

suite "day9":
  test "parseInstruction":
    check: parseInstruction("103") == (@[Mode.immediate, Mode.position,
                                         Mode.position], Opcode.input)
    check: parseInstruction("203") == (@[Mode.relative, Mode.position,
                                         Mode.position], Opcode.input)
    check: parseInstruction("204") == (@[Mode.relative, Mode.position,
                                         Mode.position], Opcode.output)
    check: parseInstruction("109") == (@[Mode.immediate, Mode.position,
                                         Mode.position], Opcode.adjustRelativeBase)

  test "extendProgram":
    let
      tp1 = @[-1]
      tp2 = @[1, 2, 3, 4]
    check: extendProgram(tp1, 4) == @[-1, 0, 0, 0]
    check: extendProgram(tp2, 2) == @[1, 2, 3, 4]

  test "computer0":
    let
      emptyInput = newSeq[int]()
      program0 = stringToProgram("109,19,204,-34")
    var
      computer0 = IntcodeComputer(program: program0, relativeBase: 2000)
      inputCounter0 = 0
    computer0 = computer0.step(emptyInput, ret, inputCounter0)
    check: computer0.instructionPointer == 2
    check: computer0.relativeBase == 2019
    computer0 = computer0.step(emptyInput, ret, inputCounter0)
    check: computer0.instructionPointer == 4
    check: computer0.relativeBase == 2019
    check: getPtr(program0, 3, Mode.relative, 2019) == 1985

  test "redditQuine":
    let emptyInput = newSeq[int]()
    # Program taken from
    # https://www.reddit.com/r/adventofcode/comments/eaboz7/quine_for_preday9_intcode_computer/
    let redditQuineInput = stringToProgram("4,44,8,1,35,41,5,41,36,1,9,1,1,5,13,37,1,3,42,42,8,42,38,43,5,43,39,1,37,40,1,5,31,37,99,87,16,0,2,34,44,0,0,0,4,44,8,1,35,41,5,41,36,1,9,1,1,5,13,37,1,3,42,42,8,42,38,43,5,43,39,1,37,40,1,5,31,37,99,87,16,0,2,34,44,0,0,0")
    var
      redditQuineComputer = IntcodeComputer(program: redditQuineInput)
      redditQuineOutput = newSeq[int]()
    while not redditQuineComputer.halted:
      redditQuineComputer = redditQuineComputer.processProgram(emptyInput,
          OutputMode.halt)
      if not redditQuineComputer.halted:
        redditQuineOutput.add(redditQuineComputer.output)
    check: redditQuineInput == redditQuineOutput

  test "program1":
    let
      emptyInput = newSeq[int]()
      program1 = stringToProgram("109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99")
    var
      computer1 = IntcodeComputer(program: program1)
      computer1Output = newSeq[int]()
    while not computer1.halted:
      computer1 = computer1.processProgram(emptyInput, OutputMode.halt)
      if not computer1.halted:
        computer1Output.add(computer1.output)
    check: program1 == computer1Output

  test "programs2and3":
    let
      emptyInput = newSeq[int]()
      computer2 = IntcodeComputer(program: stringToProgram("1102,34915192,34915192,7,4,7,99,0"))
      computer3 = IntcodeComputer(program: stringToProgram("104,1125899906842624,99"))
    check: computer2.processProgram(emptyInput).output == 34915192 * 34915192
    check: computer3.processProgram(emptyInput).output == 1125899906842624

  # TODO link to where this was taken from
  test "program4":
    let
      emptyInput = newSeq[int]()
      computer4inputval = 69
      computer4input = @[computer4inputval]
      computer4program = @[109, 1, 203, 2, 204, 2, 99]
      computer4modifiedProgram = @[109, 1, 203, computer4inputval, 204, 2, 99]
    var
      computer4 = IntcodeComputer(program: computer4program)
      inputCounter = 0

    check: parseInstruction($computer4.program[computer4.instructionPointer]) == (@[immediate, position, position], adjustRelativeBase)
    computer4 = computer4.step(emptyInput, OutputMode.ret, inputCounter)
    check: computer4.instructionPointer == 2
    check: computer4.relativeBase == 1
    check: computer4.output == 0
    check: computer4.program == computer4program
    check: parseInstruction($computer4.program[computer4.instructionPointer]) == (@[relative, position, position], input)
    check: getPtr(computer4.program, computer4.instructionPointer + 1, relative, computer4.relativeBase) == 3
    check: getPtr(computer4program, 2 + 1, relative, 1) == 3
    computer4 = computer4.step(computer4input, OutputMode.ret, inputCounter)
    check: computer4.instructionPointer == 4
    check: computer4.relativeBase == 1
    check: computer4.output == 0
    check: computer4.program == computer4modifiedProgram
    check: parseInstruction($computer4.program[computer4.instructionPointer]) == (@[relative, position, position], output)
    computer4 = computer4.step(emptyInput, OutputMode.ret, inputCounter)
    check: computer4.instructionPointer == 6
    check: computer4.relativeBase == 1
    check: computer4.output == computer4inputval

when isMainModule:
  let
    f = open("day9_input.txt")
    unprocessedProgram = readLine(f).stringToProgram()
  close(f)
  echo "part 1: ", run(unprocessedProgram, 1)
  echo "part 2: ", run(unprocessedProgram, 2)
  echo timeGo(run(unprocessedProgram, 1))
  echo timeGo(run(unprocessedProgram, 2))
