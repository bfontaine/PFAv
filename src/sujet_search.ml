type ('a, 'b) sum = Left of 'a | Right of 'b
type 'a stream = Stream of (unit -> ('a * 'a stream) option)

let nil =
  Stream(fun () -> None)

let cons el stream =
  Stream(fun () -> Some (el, stream))

let lcons el fn =
  cons el (fn ())

let get =
  fun Stream(f) -> f ()

let take s n =
  if n <= 0 then []
  else
    let rec take_r s n acc =
      let Stream(res) = s in
        match res () with
        | None -> acc
        | Some(x, s') -> take_r s' (n - 1) x::acc
    in List.reverse (take_r s n [])

let number_stream =
  (* infinite stream [0, 1, 2, 3, ...] *)
  Stream(fun () -> None) (* TODO *)

(*

let stream_of_list l =
  TODO

*)
