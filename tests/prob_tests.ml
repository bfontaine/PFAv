open OUnit
open Sujet_prob

let test_rand1 () =
  let r = (Prob.rand 1) in
    assert_equal (Prob.run compare r) [(0, 1.0)]

let test_rand2 () =
  let r = (Prob.rand 2) in
    assert_equal (Prob.run compare r) [(0, 0.5); (1, 0.5)]

let test_rand3 () =
  let r = (Prob.rand (-42)) in
    assert_equal (Prob.run compare r) []

let test_prod1 () =
  let r = (Prob.rand 1) in
    assert_equal (Prob.run compare (Prob.prod r r)) [((0, 0), 1.0)]

let test_prod2 () =
  let r = (Prob.rand 2) in
    assert_equal
      (Prob.run compare (Prob.prod r r))
      [((0,0), 0.25); ((0,1), 0.25); ((1,0), 0.25); ((1,1), 0.25)]

let test_sum1 () =
  let r = (Prob.rand 2) in
    assert_equal
      (Prob.run compare (Prob.map (fun x -> x+1) r))
      [(1, 0.5); (2, 0.5)]

let test_return1 () =
  let n = 42 in
    assert_equal (Prob.run compare (Prob.return n)) [(n,1.0)]

let test_uniform1 () =
  let a = Prob.uniform [|17;42|] in
    assert_equal (Prob.run compare a) [(17,0.5); (42, 0.5)]

let test_bind1 () =
  (* TODO Prob.bind tests *)
  ()

let suite =
  "Prob tests" >:::
    ["test_rand1"    >:: test_rand1;
     "test_rand2"    >:: test_rand2;
     "test_rand3"    >:: test_rand3;
     "test_prod1"    >:: test_prod1;
     "test_prod2"    >:: test_prod2;
     "test_sum1"     >:: test_sum1;
     "test_return1"  >:: test_return1;
     "test_uniform1" >:: test_uniform1;
     "test_bind1"    >:: test_bind1;
    ]

let _ =
  run_test_tt_main suite
