type ('st, 'a) t = 'st -> 'st * ('a, string) Result.t

let return x : _ = fun st -> st, Result.ok x
let fail err st = st, Result.error err

let ( >>= ) : 's 'a 'b. ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t =
  fun l r : _ ->
  fun st ->
  let st, x = l st in
  match x with
  | Result.Ok x -> r x st
  | Result.Error s -> st, Result.error s
;;

let read : ('st, 'st) t = fun st -> st, Result.ok st
let write : 'st -> ('st, unit) t = fun s _oldstate -> s, Result.ok ()
let run (f : ('st, 'a) t) : 'st -> 'st * ('a, string) Result.t = f
let ( let* ) = ( >>= )
let ( *> ) l r = l >>= fun _ -> r
let ( >>| ) l r = l >>= fun x -> return (r x)
let ( <* ) l r = l >>= fun h -> r >>= fun _ -> return h

let map_list : 'a 'b 's. ('a -> ('st, 'b) t) -> 'a list -> ('st, 'b list) t =
  fun custom_f mlst ->
  let f lst res =
    let* tl = lst in
    let* x = custom_f res in
    return (x :: tl)
  in
  List.fold_left f (return []) mlst >>| List.rev
;;
