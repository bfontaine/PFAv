open OUnit
open Sujet_search

(* tests helpers *)

let pint = string_of_int
let pflt = string_of_float
let pstr (s:string) = s 

let plst p_el li =
  let rec p_els xs =
    match xs with
    | [] -> ""
    | el::[] ->
        (p_el el)
    | el::xs' ->
        (p_el el) ^ ", " ^ (p_els xs')
  in
    "[" ^ (p_els li) ^ "]"

(* lists of unknown types *)
let plst2 li = 
  let rec p_els xs =
    match xs with
    | [] -> ""
    | el::[] -> "?"
    | el::xs' -> "?, "
  in
    "[" ^ (p_els li) ^ "]"

let plint = plst pint
let plstr = plst pstr
let plflt = plst pflt

let psome = function
  | None -> "None"
  | _    -> "Some(...)"

(* (int * int) list *)
let plintp = plst (fun (x,y) ->
  "(" ^ (pint x) ^ "," ^ (pint y) ^ ")")

let assert_success () =
  assert_bool "" true

let assert_solved got expected =
  assert_equal ~printer:plintp
    (List.sort compare got) (List.sort compare expected)

(* common values *)

let s1 =
  Stream(fun () ->
    Some(42, Stream(fun () ->
      None)))

let l1 =
  [42;2;45;-3;17]

let l2 =
  ["foo";"bar";"trololol"]

(* quick helpers for lazy people *)
let mk_stream1 x =
  Stream(fun () -> Some(x, Stream(fun () -> None)))

let mk_stream2 x y =
  Stream(fun () -> Some(x, mk_stream1 y))

let mk_stream3 x y z =
  Stream(fun () -> Some(x, mk_stream2 y z))

(*** Stream basics ***)

let test_nil () =
  let Stream(f) = nil in
    assert_equal ~printer:psome None (f ())

let test_cons () =
  let n = 3 in
    let Stream(f) = cons n s1 in
      match (f ()) with
      | Some(n, _) -> assert_success ()
      | _ -> assert_failure "expected Some(3, ...)"

let test_one () =
  ()
(* FIXME "unbound value 'one'"
  let n = 42 in
    let Stream(f) = one n in
      match (f ()) with
      | Some(n, Stream(s)) ->
          match s () with
          | None -> assert_success ()
          | _ -> assert_failure "expected None"
      | _ -> assert_failure "expected Some(...)"
*)

let test_lcons () =
  let n = 3 in
    let Stream(f) = lcons n (fun () -> s1) in
      match (f ()) with
      | Some(n, _) -> assert_success ()
      | _ -> assert_failure "expected Some(3, ...)"

let test_get_empty () =
  assert_equal ~printer:psome (get nil) None

let test_number_stream () =
  match get number_stream with
  | Some(x, _) -> assert_equal 0 x
  | _ -> assert_failure "expected Some(0, ...)"

let test_take () =
  assert_equal ~printer:plint [] (take nil 0);
  assert_equal ~printer:plint [] (take nil 3);
  assert_equal ~printer:plint [] (take s1  0);
  assert_equal ~printer:plint [0;1;2] (take number_stream 3)

let test_stream_of_list () =
  assert_equal ~printer:plint l1 (take (stream_of_list l1) 20);
  assert_equal ~printer:plstr l2 (take (stream_of_list l2) 20)

(*** Logic1 ***)

let test_logic1_fail () =
  assert_equal ~printer:plst2 [] (Logic1.solve 0 Logic1.fail);
  assert_equal ~printer:plst2 [] (Logic1.solve 4 Logic1.fail)

let test_logic1_return () =
  let n = 3 in
    assert_equal ~printer:plint [n] (Logic1.solve 1 (Logic1.return n))

let test_logic1_stream () =
  assert_equal ~printer:plint
    l1 (Logic1.solve 20 (Logic1.stream (stream_of_list l1)))

let test_logic1_map () =
  let f = (fun x -> x * 2) in
    let s = Logic1.map f (Logic1.stream number_stream) in
      assert_equal ~printer:plint [0;2;4;6] (Logic1.solve 4 s)

let test_logic1_guard () =
  let g = (fun x -> x mod 3 == 0) in
    let s = Logic1.guard g (Logic1.stream number_stream) in
      assert_equal ~printer:plint [0;3;6;9] (Logic1.solve 4 s)

let test_logic1_sum () =
  let s1 = Logic1.stream (stream_of_list l1)
  and s2 = Logic1.stream (stream_of_list l2) in
    let s3 = Logic1.sum s1 s2 in
      assert_equal ~printer:pint
        ((List.length l1) + List.length l2)
        (List.length (Logic1.solve 100 s3))

let test_logic1_prod_fail () =
  let s = Logic1.stream s1
  and f = Logic1.fail in
    begin
      (* fail on the left and right *)
      let p = Logic1.prod f f in
        assert_equal ~printer:plintp [] (Logic1.solve 20 p);
      (* fail on the left *)
      let p = Logic1.prod f s in
        assert_equal ~printer:plintp [] (Logic1.solve 20 p);
      (* fail on the right *)
      let p = Logic1.prod s f in
        assert_equal ~printer:plintp [] (Logic1.solve 20 p);
    end

let test_logic1_prod_1x1 () =
  let s =
    Logic1.stream (mk_stream1 42)
  in
    assert_equal ~printer:plintp [(42,42)] (Logic1.solve 20 (Logic1.prod s s))

let test_logic1_prod_1x2 () =
  let s1 = mk_stream1 42
  and s2 = mk_stream2 17 42
  in
    let p =
      Logic1.prod (Logic1.stream s1) (Logic1.stream s2)
    in
      assert_solved [(42,17);(42,42)] (Logic1.solve 20 p)

let test_logic1_prod_2x1 () =
  let s1 = mk_stream2 17 42
  and s2 = mk_stream1 42
  in
    let p =
      Logic1.prod (Logic1.stream s1) (Logic1.stream s2)
    in
      assert_solved [(17,42);(42,42)] (Logic1.solve 20 p)

let test_logic1_prod_2x2 () =
  let s1 = mk_stream2 1 2
  and s2 = mk_stream2 3 4
  in
    let p =
      Logic1.prod (Logic1.stream s1) (Logic1.stream s2)
    in
      assert_solved [(1,3);(1,4);(2,3);(2,4)] (Logic1.solve 20 p)

let test_logic1_prod_infxinf () =
  let n = Logic1.stream number_stream in
    let p = Logic1.guard (fun (x,_) -> x = 1) (Logic1.prod n n) in
      assert_solved [(1,0);(1,1);(1,2)] (Logic1.solve 3 p)

let test_logic1_prod_2x3 () =
  let s1 = mk_stream2 1 2
  and s2 = mk_stream3 3 4 5
  in
    let p =
      Logic1.prod (Logic1.stream s1) (Logic1.stream s2)
    in
      assert_solved
        [(1,3);(1,4);(1,5);(2,3);(2,4);(2,5)] (Logic1.solve 20 p)

let test_logic1_prod_3xinf () =
  let s1 = mk_stream3 1 2 3
  and s2 = number_stream
  in
    let p =
      Logic1.guard (fun (_,y) -> y = 1)
        (Logic1.prod (Logic1.stream s1) (Logic1.stream s2))
    in
      assert_solved [(1,1);(2,1);(3,1)] (Logic1.solve 20 p)

let test_logic1_prod_infx3 () =
  let s1 = number_stream
  and s2 = mk_stream3 1 2 3
  in
    let p =
      Logic1.guard (fun (x,_) -> x = 1)
        (Logic1.prod (Logic1.stream s1) (Logic1.stream s2))
    in
      assert_solved [(1,1);(1,2);(1,3)] (Logic1.solve 3 p)

let test_logic1_prod_3x2 () =
  let s1 = mk_stream3 1 2 3
  and s2 = mk_stream2 4 5
  in
    let p =
      Logic1.prod (Logic1.stream s1) (Logic1.stream s2)
    in
      assert_solved
        [(1,4);(1,5);(2,4);(2,5);(3,4);(3,5)] (Logic1.solve 20 p)

(* teacher's test *)
let test_logic1 () =
  let open Logic1 in
  let number = stream number_stream in
  let posnum = guard (fun n -> n > 0) number in

  let pytha =
    (* we search for triples pythagorean triples (a, b, c):
         a² + b² = c²
       to avoid duplication of solutions, we also request (a < b) *)
    let ab =
      prod posnum posnum
      |> guard (fun (a,b) -> a < b) in
    prod ab posnum
    |> guard (fun ((a,b),c) -> a*a+b*b=c*c) in

  ignore (solve 10 pytha)

(**************)

let suite =
  "Stream tests" >:::
    ["test_nil"                 >:: test_nil;
     "test_cons"                >:: test_cons;
     "test_one"                 >:: test_one;
     "test_lcons"               >:: test_lcons;
     "test_get_empty"           >:: test_get_empty;
     "test_number_stream"       >:: test_number_stream;
     "test_take"                >:: test_take;
     "test_stream_of_list"      >:: test_stream_of_list;

     "test_logic1_fail"         >:: test_logic1_fail;
     "test_logic1_return"       >:: test_logic1_return;
     "test_logic1_stream"       >:: test_logic1_stream;
     "test_logic1_map"          >:: test_logic1_map;
     "test_logic1_guard"        >:: test_logic1_guard;
     "test_logic1_sum"          >:: test_logic1_sum;

     "test_logic1_prod_fail"    >:: test_logic1_prod_fail;

     "test_logic1_prod_1x1"     >:: test_logic1_prod_1x1;
     "test_logic1_prod_2x2"     >:: test_logic1_prod_2x2;

     "test_logic1_prod_1x2"     >:: test_logic1_prod_1x2;
     "test_logic1_prod_2x1"     >:: test_logic1_prod_2x1;
     "test_logic1_prod_2x3"     >:: test_logic1_prod_2x3;
     "test_logic1_prod_3x2"     >:: test_logic1_prod_3x2;

     "test_logic1_prod_3xinf"   >:: test_logic1_prod_3xinf;
     "test_logic1_prod_infx3"   >:: test_logic1_prod_infx3;

     "test_logic1_prod_infxinf" >:: test_logic1_prod_infxinf;

     "test_logic1"              >:: test_logic1;
    ]

let _ =
  run_test_tt_main suite
