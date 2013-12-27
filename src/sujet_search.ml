type ('a, 'b) sum = Left of 'a | Right of 'b
type 'a stream = Stream of (unit -> ('a * 'a stream) option)

let nil =
  Stream(fun () -> None)

let cons el stream =
  Stream(fun () -> Some (el, stream))

let one el =
  (cons el nil)

let lcons el fn =
  cons el (fn ())

let get =
  fun (Stream f) -> f ()

let take s n =
  let rec take_r s n acc =
    if n <= 0 then acc
    else
      let Stream(res) = s in
        match res () with
        | None -> acc
        | Some(x, s') -> take_r s' (n - 1) (x::acc)
  in List.rev (take_r s n [])

let number_stream =
  let rec next_number n =
    Stream(fun () -> Some (n, next_number (n+1)))
  in
    next_number 0

let stream_of_list l =
  List.fold_right (fun el st -> cons el st) l nil

module Logic1 = struct
  type 'a search = Search of 'a stream

  let return x = Search (one x)

  let fail = Search nil

  let stream st = Search st

  let solve = fun n (Search st) ->
    take st n

  let keep_if stream test =
    (* TODO *)
    stream

  let map_stream fn stream =
    (* TODO *)
    stream

  let map = fun fn (Search st) ->
    Search(map_stream fn st)

  let guard = fun test (Search st) ->
    Search(keep_if st test)
(*
  val map : ('a -> 'b) -> 'a search -> 'b search
  (** the solutions of [map f prob] are all the [f x] such that [x]
      is a solution of [prob]. *)

  val sum : 'a search -> 'b search -> ('a, 'b) sum search
  (** the solutions of [sum pa pb] are all the solutions of problem
      [pa], and all the solutions of problem [pb] *)

  val prod : 'a search -> 'b search -> ('a * 'b) search

  val guard : ('a -> bool) -> 'a search -> 'a search
  (** the solutions of [guard condition prob] are the solutions of
      [prob] that also satisfy [condition]. *)
*)
end
