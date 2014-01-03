open OUnit
open Sujet_search

let assert_success () =
  assert_bool "" true

let assert_sorted_eq l1 l2 =
  assert_equal (List.sort compare l1) (List.sort compare l2)

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
    assert_equal (f ()) None

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

let test_get () =
  assert_equal (get nil) None

let test_number_stream () =
  match get number_stream with
  | Some(x, _) -> assert_equal x 0
  | _ -> assert_failure "expected Some(0, ...)"

let test_take () =
  assert_equal (take nil 0) [];
  assert_equal (take nil 3) [];
  assert_equal (take s1  0) [];
  assert_equal (take number_stream 3) [0;1;2]

let test_stream_of_list () =
  assert_equal (take (stream_of_list l1) 20) l1;
  assert_equal (take (stream_of_list l2) 20) l2

(*** Logic1 ***)

let test_logic1_fail () =
  assert_equal (Logic1.solve 0 Logic1.fail) [];
  assert_equal (Logic1.solve 4 Logic1.fail) []

let test_logic1_return () =
  let n = 3 in
    assert_equal (Logic1.solve 1 (Logic1.return n)) [n]

let test_logic1_stream () =
  assert_equal
    (Logic1.solve 20 (Logic1.stream (stream_of_list l1))) l1

let test_logic1_map () =
  let f = (fun x -> x * 2) in
    let s = Logic1.map f (Logic1.stream number_stream) in
      assert_equal (Logic1.solve 4 s) [0;2;4;6]

let test_logic1_guard () =
  let g = (fun x -> x mod 3 == 0) in
    let s = Logic1.guard g (Logic1.stream number_stream) in
      assert_equal (Logic1.solve 4 s) [0;3;6;9]

let test_logic1_sum () =
  let s1 = Logic1.stream (stream_of_list l1)
  and s2 = Logic1.stream (stream_of_list l2) in
    let s3 = Logic1.sum s1 s2 in
      assert_equal
        (List.length (Logic1.solve 100 s3))
        ((List.length l1) + (List.length l2))

let test_logic1_prod_fail () =
  let s = Logic1.stream s1
  and f = Logic1.fail in
    begin
      (* fail on the left and right *)
      let p = Logic1.prod f f in
        assert_equal (Logic1.solve 20 p) [];
      (* fail on the left *)
      let p = Logic1.prod f s in
        assert_equal (Logic1.solve 20 p) [];
      (* fail on the right *)
      let p = Logic1.prod s f in
        assert_equal (Logic1.solve 20 p) [];
    end

let test_logic1_prod_1x1 () =
  let s =
    Logic1.stream (mk_stream1 42)
  in
    assert_equal (Logic1.solve 20 (Logic1.prod s s)) [(42,42)]

let test_logic1_prod_1x2 () =
  let s1 = mk_stream1 42
  and s2 = mk_stream2 17 42
  in
    let p =
      Logic1.prod (Logic1.stream s1) (Logic1.stream s2)
    in
      assert_sorted_eq (Logic1.solve 20 p) [(42,17);(42,42)]

let test_logic1_prod_2x1 () =
  let s1 = mk_stream2 17 42
  and s2 = mk_stream1 42
  in
    let p =
      Logic1.prod (Logic1.stream s1) (Logic1.stream s2)
    in
      assert_sorted_eq (Logic1.solve 20 p) [(17,42);(42,42)]

let test_logic1_prod_2x2 () =
  let s1 = mk_stream2 1 2
  and s2 = mk_stream2 3 4
  in
    let p =
      Logic1.prod (Logic1.stream s1) (Logic1.stream s2)
    in
      assert_sorted_eq (Logic1.solve 20 p) [(1,3);(1,4);(2,3);(2,4)]

let test_logic1_prod_infxinf () =
  let n = Logic1.stream number_stream in
    let p = Logic1.guard (fun (x,_) -> x = 1) (Logic1.prod n n) in
      assert_sorted_eq (Logic1.solve 3 p) [(1,0);(1,1);(1,2)]

let test_logic1_prod_2x3 () =
  let s1 = mk_stream2 1 2
  and s2 = mk_stream3 3 4 5
  in
    let p =
      Logic1.prod (Logic1.stream s1) (Logic1.stream s2)
    in
      assert_sorted_eq
        (Logic1.solve 20 p) [(1,3);(1,4);(1,5);(2,3);(2,4);(2,5)]

let test_logic1_prod_3x2 () =
  let s1 = mk_stream3 1 2 3
  and s2 = mk_stream2 4 5
  in
    let p =
      Logic1.prod (Logic1.stream s1) (Logic1.stream s2)
    in
      assert_sorted_eq
        (Logic1.solve 20 p) [(1,4);(1,5);(2,4);(2,5);(3,4);(3,5)]

(**************)

let suite =
  "Stream tests" >:::
    ["test_nil"                 >:: test_nil;
     "test_cons"                >:: test_cons;
     "test_one"                 >:: test_one;
     "test_lcons"               >:: test_lcons;
     "test_get"                 >:: test_get;
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
     "test_logic1_prod_infxinf" >:: test_logic1_prod_infxinf;
    ]

let _ =
  run_test_tt_main suite
