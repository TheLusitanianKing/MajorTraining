open Base
open Model

let drop_exercise exs ex =
  let rec helper acc exs' = match exs' with
    | [] -> List.rev acc
    | (x::xs) when (phys_equal x ex) -> helper acc xs
    | (x::xs) -> helper (x::acc) xs
  in helper [] exs

let valid_exercise_for_step step ex =
  all (fun ex_eq -> List. ()) (exercise_equipments ex)

let generate steps exercises nb_rounds =
  Random.self_init ();
  let rec helper acc tmp_acc sts exs n =
    if n <= 0 then { circuit_rounds = acc }
    else
      match (sts, exs) with
        | ([], _)      -> helper (tmp_acc :: acc) [] steps exs (n - 1)
        | (_, [])      -> failwith "Not enough exercises to feed." (* ! *)
        | ((s::ss), _) ->
          let valid_exercises = List.filter ~f:(valid_exercise_for_step s) exs in
          let nb_exs = List.length valid_exercises in
          let i = Random.int nb_exs in
          let ex = List.nth_exn valid_exercises i in
          let rem_exs = drop_exercise exs ex in
          helper acc ((s, ex) :: tmp_acc) ss rem_exs nb_rounds
  in helper [] [] steps exercises nb_rounds