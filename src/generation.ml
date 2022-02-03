open Base
open Model

let drop_i ls i =
  let rec helper acc ls' i' = match ls' with
    | [] -> List.rev acc
    | (_::xs) when (i' = 0) -> helper acc xs (i' - 1)
    | (x::xs) -> helper (x::acc) xs (i' - 1)
  in helper [] ls i

let generate steps exercises nb_rounds =
  Random.self_init ();
  let rec helper acc tmp_acc sts exs n =
    if n <= 0 then { circuit_rounds = acc }
    else
      match (sts, exs) with
        | ([], _)      -> helper (tmp_acc :: acc) [] steps exs (n - 1)
        | (_, [])      -> failwith "Not enough exercises to feed." (* ! *)
        | ((s::ss), _) ->
          let nb_exs = List.length exs in
          let i = Random.int nb_exs in
          let ex = List.nth_exn exs i in
          let rem_exs = drop_i exs i in
          helper acc ((s, ex) :: tmp_acc) ss rem_exs nb_rounds
  in helper [] [] steps exercises nb_rounds