module Generation 
  ( generateCircuit
  )
where

import Model
import System.Random

generateCircuit :: StdGen -> [Exercise] -> Int -> Circuit -> GeneratedCircuit
generateCircuit gen exs nbRounds c =
  helper [[]] [] exs (circuitSteps c) gen nbRounds
  where
    helper acc tmp es steps g 0 = GeneratedCircuit { circuit = c, rounds = undefined }
    helper acc tmp es steps g n = undefined

-- let generate steps exercises nb_rounds =
--   Random.self_init ();
--   let rec helper acc tmp_acc sts exs n =
--     if n <= 0 then { circuit_rounds = acc }
--     else
--       match (sts, exs) with
--         | ([], _)      -> helper (tmp_acc :: acc) [] steps exs (n - 1)
--         | (_, [])      -> failwith "Not enough exercises to feed." (* ! *)
--         | ((s::ss), _) ->
--           let valid_exercises = List.filter ~f:(valid_exercise_for_step s) exs in
--           let nb_exs = List.length valid_exercises in
--           let i = Random.int nb_exs in
--           let ex = List.nth_exn valid_exercises i in
--           let rem_exs = drop_exercise exs ex in
--           helper acc ((s, ex) :: tmp_acc) ss rem_exs nb_rounds
--   in helper [] [] steps exercises nb_rounds