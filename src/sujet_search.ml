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

  let map = fun fn (Search st) ->
    let rec map_stream fn stream =
      match stream with
      | Stream(stf) ->
          match stf () with
          | None -> nil
          | Some(x, stf') ->
              Stream(fun () -> Some(fn x, map_stream fn stf'))
    in
      Search(map_stream fn st)

  let guard = fun test (Search st) ->
    let rec keep_if stream test =
      match stream with
      | Stream(stf) ->
          match stf () with
            | None -> nil
            | Some(x, stf') ->
                if (test x)
                then Stream(fun () -> Some(x, keep_if stf' test))
                (* FIXME infinite loop if (text x) is false *)
                else (keep_if stf' test)
    in
      Search(keep_if st test)

  let sum = fun (Search st1) (Search st2) ->
    Search(nil) (* TODO *)

  let prod = fun (Search st1) (Search st2) ->
    (* FIXME this is a wrong implementation,
     * prod (1,2) (1,2) should not give ((1,1),(2,2)) but
     *  ((1,1),(1,2),(2,1),(2,2)) instead, that is for each element
     *  of st1 and for each element of st2 create a pair of both.
     *
     * I'm not sure how this is useful on infinite streams, e.g.:
     *  prod (1,2,...) (1,2,...) -> ((1,1),(1,2),...) and it'll never
     *   give pairs with a non-1 number on the left.
     *)
    let rec prod_stream (Stream str1) (Stream str2) =
      match str1 (), str2 () with
      | None, _ -> nil
      | _, None -> nil
      | Some(x1, str1'), Some(x2, str2') ->
          Stream(fun () ->
            Some((x1, x2), prod_stream str1' str2'))
    in
      Search(prod_stream st1 st2)

(*

  val sum : 'a search -> 'b search -> ('a, 'b) sum search
  (** the solutions of [sum pa pb] are all the solutions of problem
      [pa], and all the solutions of problem [pb] *)
*)
end
