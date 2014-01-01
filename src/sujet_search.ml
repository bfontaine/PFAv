type ('a, 'b) sum = Left of 'a | Right of 'b
type 'a stream = Stream of (unit -> ('a * 'a stream) option)

let nil =
  Stream(fun () -> None)

let cons el stream =
  Stream(fun () -> Some (el, stream))

let one el =
  cons el nil

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
      let Stream(stf) = stream in
        match stf () with
        | None -> nil
        | Some(x, stf') ->
            Stream(fun () -> Some(fn x, map_stream fn stf'))
    in
      Search(map_stream fn st)

  let guard = fun test (Search st) ->
    let rec keep_if stream test =
      let Stream(stf) = stream in
        match stf () with
        | None -> nil
        | Some(x, stf') ->
            if (test x)
            then Stream(fun () -> Some(x, keep_if stf' test))
            else (keep_if stf' test)
    in
      Search(keep_if st test)

  let sum = fun (Search st1) (Search st2) ->
    let rec sum_stream st1 st2 =
      let Stream(s1), Stream(s2) = st1, st2 in
        match s1 (), s2 () with
        | None, None -> nil
        | Some(x, st1'), None ->
            Stream(fun() -> Some(Left x, sum_stream st1' st2))
        | None, Some(x, st2') ->
            Stream(fun() -> Some(Right x, sum_stream st1 st2'))
        | Some(x, st1'), Some(y, st2') ->
            Stream(fun() ->
              Some(Left x,
                Stream(fun () ->
                  Some(Right y, sum_stream st1' st2'))))
    in
      Search(sum_stream st1 st2)

  (* The idea is to stream the product of two series by 'diagonals', as if
   * the pairs were represented in a matrix as below:
   *
   * .     A     B     C     D
   * X (X,A) (X,B) (X,C) (X,D)
   * Y (Y,A) (Y,B) (Y,C) (Y,D)
   * Z (Z,A) (Z,B) (Z,C) (Z,D)
   *
   * This gives us something like that:
   *
   *  (X,A) 
   *  (X,B) (Y,A)
   *  (X,C) (Y,B) (Z,A)
   *  (X,D) (Y,C) (Z,B)
   *  (Y,D) (Z,C)
   *  (Z,D)
   *
   * The algorithm in pseudo-code:
   *
   * stack = []
   * for x_i in S1:
   *   stack << x_i
   *   stack2 = stack
   *   for y_j in S2 AND while stack2 not empty:
   *     stream (x_i, y_j)
   *     pop stack2
   *
   * The issue is that we re-compute S2's solutions for each S1's one.
   *
   * Example:
   *
   * with S1 = [1,2,3,4]
   *      S2 = [A,B]
   *
   * stack = [1]
   * (1,A)
   * stack = [1,2]
   * (2,A)
   * (1,B)
   * stack = [1,2,3]
   * (3,A)
   * (2,B)
   * stack = [1,2,3,4]
   * (4,A)
   * (3,B)
   *       --> problem here, (4,B) is missing (FIXME)
   *           this is an issue only for finite streams
   **)
  let prod = fun (Search st1) (Search st2) ->
    let rec prod_stream st1 st2 stack =
      let Stream(s1) = st1 in
        match s1 () with
        | None -> nil
        | Some (x, st1') ->
            let stack' = x::stack in
              let rec for_stack st stck =
                match stck with
                | [] -> prod_stream st1' st2 stack'
                | el::stck' ->
                    let Stream(s) = st in
                      match s () with
                      | None -> prod_stream st1' st2 stack'
                      | Some(y, st') ->
                          Stream(fun () ->
                            Some((el, y), for_stack st' stck'))
              in
                for_stack st2 stack'
    in
      Search(prod_stream st1 st2 [])

end
