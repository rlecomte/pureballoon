module Model exposing (grid, Cell)

import Random
import Date
import Task

type alias Cell =
  { row: Int,
    column: Int,
    caseType: Int
  }

initSeed: Task.Task a Random.Seed
initSeed = Date.now
  |> Task.map(Date.millisecond)
  |> Task.map(Random.initialSeed)

randomInts: Int -> Random.Seed -> List Int
randomInts n seed =
  let (x,s) = Random.step (Random.int 0 5) seed
  in if(n > 0) then x :: randomInts (n - 1) s else []

grid: Task.Task a (List Cell)
grid = initSeed
  |> Task.map(randomInts 63)
  |> Task.map(List.indexedMap(\i r -> { row = (i % 7), column = (i % 9), caseType = r}))
