import algorithm
import sequtils
import sets
import strutils
import tables
import unittest
import ./aoc_utils

func makeCandidatesMinus(joltage: int, minDiff: int = 1, maxDiff: int = 3): seq[int] =
  for toDiff in minDiff..maxDiff:
    let candidate = joltage - toDiff
    if candidate >= 0:
      result.add candidate

func makeCandidates(joltage: int, minDiff: int = 1, maxDiff: int = 3): seq[int] =
  for diff in minDiff..maxDiff:
    result.add joltage + diff

func getDifferences(adapters: seq[int]): Table[int, int] =
  result[1] = 0
  result[3] = 0
  let
    # reversedAdapterJoltages = adapters.sorted(Descending)
    # allJoltages = @[reversedAdapterJoltages[0] + 3] & reversedAdapterJoltages & @[0]
    adapterJoltages = adapters.sorted
    orderedJoltages = @[0] & adapterJoltages & @[adapterJoltages[high(adapterJoltages)] + 3]
    allJoltages = orderedJoltages.toHashSet
  # Because you have to use the closest values, you have to go in order, so
  # can't use a set.
  #
  # var
  #   allJoltages = adapters.toHashSet
  #   candidates: seq[int]
  # allJoltages.incl(adapters.foldl(max(a, b)) + 3)
  # allJoltages.incl 0
  # for joltage in allJoltages:
  #   candidates = joltage.makeCandidates
  var diff: int
  for joltage in orderedJoltages:
    for candidate in joltage.makeCandidates:
      if candidate in allJoltages:
        result[candidate - joltage] += 1
        break

func getResult(adapters: seq[int]): int =
  let diffs = adapters.getDifferences
  diffs[1] * diffs[3]

# This is where I steal an algorithm from
# https://github.com/networkx/networkx/blob/0ead17fb18334040b84dd614d1f12d6d3fa9e130/networkx/algorithms/simple_paths.py#L294
# and stick it on top of a barebones graph object type, because patgraph
# doesn't work for me.

# type
#   ## A stupid unweighted graph implementation based on adjacency lists.
#   Graph = object
#     impl: Table[int, seq[int]]

# proc contains(g: Graph, item: int): bool =
#   g.impl.contains(item)

# proc `[]`(g: Graph, key: int): seq[int] =
#   g.impl[key]

# proc `[]=`(g: Graph, key: int, val: seq[int]) =
#   g.impl[key] = val

# proc addEdge(graph: var Graph, source, target: int) =
#   if source notin graph:
#     graph[source] = @[target]
#   elif target notin graph[source]:
#     graph[source].add(target)

# proc addEdges(graph: var Graph, edges: openArray[int, int]) =
#   for (source, target) in edges.items:
#     graph.addEdge(source, target)

# proc addEdges(edges: openArray[int, int]): Graph =
#   result.addEdges edges

# func allSimplePaths

# proc toGraph[int](edges: seq[(int, int)]): Graph[int, float] =
  # -- patgraph
  # edges.graphFromEdges

proc getUnorderedDifferences(adapters: seq[int]): seq[(int, int)] =
  ## Generate all legal graph edges, then find all paths in the graph that
  ## start and terminate at fixed points.
  var
    allJoltages = adapters.toHashSet
    legalPairs: seq[(int, int)]
  let
    pathStart = 0
    pathEnd = adapters.foldl(max(a, b)) + 3
  allJoltages.incl pathStart
  allJoltages.incl pathEnd
  for joltage in allJoltages:
    for candidate in joltage.makeCandidates:
      if candidate in allJoltages:
        legalPairs.add((joltage, candidate))
  # echo legalPairs.toGraph
  legalPairs

suite "day10":
  let
    adapters1 = @[16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4]
    adapters2 = @[28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45,
                  19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3]
  test "part 1":
    check: adapters1.getDifferences == {1: 7, 3: 5}.toTable
    check: adapters2.getDifferences == {1: 22, 3: 10}.toTable
    check: adapters1.getResult == 35
    check: adapters2.getResult == 220
  # test "part 2":
  #   echo adapters1.getUnorderedDifferences.sorted


when isMainModule:
  let input = "day10_input.txt".readAllLines.mapIt(it.parseInt)
  echo "part 1: ", input.getResult
  echo "part 2: "
