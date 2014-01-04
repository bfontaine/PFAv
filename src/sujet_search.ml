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

  let rec solve = fun n (Search st) ->
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

  let guardi = fun test (Search st) ->
    let rec keep_if i stream test =
      let Stream(stf) = stream in
        match stf () with
        | None -> nil
        | Some(x, stf') ->
            if (test i x)
            then Stream(fun () -> Some(x, keep_if (i+1) stf' test))
            else (keep_if (i+1) stf' test)
    in
      Search(keep_if 0 st test)

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

  (* The .prod function took a lot of time to be correctly implemented, and
   * various strategies have been tested. The one which is implemented below
   * works with a 'diagonal': We progress through the matrice with a diagonal
   * North-East -> South-West. To be clear, here is the thought model:
   * 
   *  the carthesian product is a (possibly infinite) matrice where columns are
   *  S1's values (S1 is the first stream, S2 is the second one), and lines are
   *  S2's. For example, prod {1, 2} {"a", "b"} gives:
   *
   *  |  -S1->
   * S2  1a 2a
   *  |  1b 2b
   *  V
   *
   * We keep track of S1's elements in a stack and for each element, we add it
   * on the stack, then enter a subroutine with a copy of this stack which
   * loops on it and at each time pick S2's next element and the top of the
   * stack, create the pair and continues until S2 and/or the stack are empty.
   * Then the main routine continues to S1's next element.
   *
   * The problem of this implementation is that on products where S1 is finite,
   * pairs below the diagonale are never computed, in our previous example '2b'
   * is not in the product. I spent a few hours trying to fix this to work with
   * all kind of products, and tested other implementations, for example by
   * walking the matrice using reversed "L" (in our example, this would give us
   * 1a, then 2a, 2b, 1b), but it was not elegant and didn't work properly. I
   * then went back on my original implementation and added 'prod_finite_s1'
   * which is called when we reach S1's end. It takes the stack of its
   * elements, and it add the missing elements to the product. It also works if
   * S1 is finite but not S2.
   **)

  (** this is an helper for 'prod', see the comment above *)
  let prod_finite_s1 stck s2 =

    let rec line els1 val2 next =
      match els1 with
      | [] -> next ()
      | e::els1' ->
          Stream(fun () ->
            Some((e, val2), line els1' val2 next))

    and each_line stk s2 els =
      match (let Stream(s) = s2 in s()) with
      | None -> nil
      | Some(y, s2') ->
          line els y (fun () ->
            match stk with
            | e::stk' ->
                each_line stk' s2' (e::els)
            | [] ->
                each_line stk s2' els)

    in
      each_line stck s2 []
  
  let prod = fun (Search ss1) (Search ss2) ->
    let rec prod_stream st1 st2 stack =
      match (let Stream(s) = st1 in s ()) with
      | None ->
          (* s1 is empty => ss1 is finite *)
          prod_finite_s1 stack ss2
      | Some (x, st1') ->
          (* stack keeps a reversed list of st1's elements *)
          let stack' = x::stack in
            (* for each element, we loop on the stack to go backward on st1
             * and at the same time forward on st2 *)
            let rec for_stack st stck =
              match stck with
              (* if the stack is empty, we move to st1's next element *)
              | [] -> prod_stream st1' st2 stack'
              (* if not... *)
              | el::stck' ->
                  match (let Stream(s) = st in s ()) with
                  (* if st2 is empty, we move to st1's next element *)
                  | None -> prod_stream st1' st2 stack'
                  (* if not, we add a pair with the top of the stack
                   * and st2's first element, and continue with the rest
                   * of the stack and the rest of st2 *)
                  | Some(y, st') ->
                      Stream(fun () ->
                        Some((el, y), for_stack st' stck'))
            in
              for_stack st2 stack'
      in
        Search(prod_stream ss1 ss2 [])
end
