import lenientops
import math
import sequtils
import strutils
import tables
import ./aoc_utils
import unittest

type
  Motion = enum
    moveNorth, moveSouth, moveEast, moveWest, rotLeft, rotRight, goForward
  Instruction = tuple
    motion: Motion
    count: int
  Direction = enum
    north, south, east, west
  Point = tuple
    x: int
    y: int
  Ship = tuple
    facing: Direction
    pos: Point
    wp: Point

func initShip(): Ship =
  (east, (0, 0), (10, 1))

func parseInstruction(instruction: string): Instruction =
  let
    num = instruction[1..^1].parseInt
    motionChar = instruction[0]
    motion = case motionChar
             of 'N': moveNorth
             of 'S': moveSouth
             of 'E': moveEast
             of 'W': moveWest
             of 'L': rotLeft
             of 'R': rotRight
             of 'F': goForward
             else: raise
  (motion, num)

func invert[A, B](table: Table[A, B]): Table[B, A] =
  for k, v in table.pairs:
    result[v] = k

# directions are ordered clockwise
const
  directions = {north: 0, east: 1, south: 2, west: 3}.toTable
  invertedDirections = directions.invert

func rotate(compass: Direction, rotation: Motion, degrees: int): Direction =
  let
    start = directions[compass]
    newIndex = case rotation
               of rotLeft: (start - int(degrees / 90)).floorMod(4)
               of rotRight: (start + int(degrees / 90)).floorMod(4)
               else: raise
  invertedDirections[newIndex]

func rotatePoint(point: Point; degrees: int): Point =
  ## for a point (x, y), rotating it theta degrees gives
  ##  x' = x * cos(theta) - y * sin(theta)
  ##  y' = x * sin(theta) + y * cos(theta)
  ## where theta is positive for CCW/left and negative for CW/right.
  # let theta = degrees.float.degToRad
  # (x: int(point.x * theta.cos - point.y * theta.sin),
  #  y: int(point.x * theta.sin + point.y * theta.cos))
  case degrees
  of 90: (-point.y, point.x)
  of 180: (-point.x, -point.y)
  of 270: (point.y, -point.x)
  of -90: (point.y, -point.x)
  of -180: (-point.x, -point.y)
  of -270: (-point.y, point.x)
  else: raise

func move(ship: Ship, instruction: Instruction): Ship =
  result = ship.deepCopy
  case instruction.motion
  of moveNorth:
    result.pos.y += instruction.count
  of moveSouth:
    result.pos.y -= instruction.count
  of moveEast:
    result.pos.x += instruction.count
  of moveWest:
    result.pos.x -= instruction.count
  of rotLeft:
    result.facing = result.facing.rotate(rotLeft, instruction.count)
  of rotRight:
    result.facing = result.facing.rotate(rotRight, instruction.count)
  # move forward in the direction the ship is facing
  of goForward:
    case ship.facing
    of north:
      result.pos.y += instruction.count
    of south:
      result.pos.y -= instruction.count
    of east:
      result.pos.x += instruction.count
    of west:
      result.pos.x -= instruction.count

func moveWaypoint(ship: Ship, instruction: Instruction): Ship =
  result = ship.deepCopy
  case instruction.motion
  of moveNorth:
    result.wp.y += instruction.count
  of moveSouth:
    result.wp.y -= instruction.count
  of moveEast:
    result.wp.x += instruction.count
  of moveWest:
    result.wp.x -= instruction.count
  of rotLeft:
    result.wp = result.wp.rotatePoint(instruction.count)
  of rotRight:
    result.wp = result.wp.rotatePoint(-instruction.count)
  of goForward:
    result.pos.x += (result.wp.x * instruction.count)
    result.pos.y += (result.wp.y * instruction.count)

func manhattanDist(point: Point): int =
  ## Assume the origin is fixed at (0, 0).
  point.x.abs + point.y.abs

func followInstructions(
  ship: Ship, instructions: openArray[Instruction], waypoint: bool
): Ship =
  result = ship.deepCopy
  let mover = if waypoint: moveWaypoint else: move
  for instruction in instructions:
    result = result.mover(instruction)

suite "day12":
  let example = """
F10
N3
F7
R90
F11""".splitlines.mapIt(it.parseInstruction)
  test "part 1":
    check: example == @[
      (goForward, 10),
      (moveNorth, 3),
      (goForward, 7),
      (rotRight, 90),
      (goForward, 11)
    ]
    check: north.rotate(rotLeft, 270) == east
    check: west.rotate(rotLeft, 270) == north
    check: north.rotate(rotRight, 270) == west
    check: west.rotate(rotRight, 270) == south
    check: north.rotate(rotLeft, 0) == north
    check: north.rotate(rotRight, 0) == north
    let ship0 = initShip()
    let ship1 = ship0.move(example[0])
    check: ship1 == (east, (10, 0), (10, 1))
    let ship2 = ship1.move(example[1])
    check: ship2 == (east, (10, 3), (10, 1))
    let ship3 = ship2.move(example[2])
    check: ship3 == (east, (17, 3), (10, 1))
    let ship4 = ship3.move(example[3])
    check: ship4 == (south, (17, 3), (10, 1))
    let ship5 = ship4.move(example[4])
    check: ship5 == (south, (17, -8), (10, 1))
    check: ship5.pos.manhattanDist == 25
  test "part 2":
    check: (10, 4).rotatePoint(-90) == (4, -10)
    let ship0 = initShip()
    let ship1 = ship0.moveWaypoint(example[0])
    check: ship1 == (east, (100, 10), (10, 1))
    let ship2 = ship1.moveWaypoint(example[1])
    check: ship2 == (east, (100, 10), (10, 4))
    let ship3 = ship2.moveWaypoint(example[2])
    check: ship3 == (east, (170, 38), (10, 4))
    let ship4 = ship3.moveWaypoint(example[3])
    check: ship4 == (east, (170, 38), (4, -10))
    let ship5 = ship4.moveWaypoint(example[4])
    check: ship5 == (east, (214, -72), (4, -10))
    check: ship5.pos.manhattanDist == 286

when isMainModule:
  let input = "day12_input.txt".readAllLines.mapIt(it.parseInstruction)
  echo "part 1: ", initShip().followInstructions(input, false).pos.manhattanDist
  echo "part 2: ", initShip().followInstructions(input, true).pos.manhattanDist
