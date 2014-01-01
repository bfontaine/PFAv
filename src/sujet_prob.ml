(** {1 Second part: random computations} *)

(** The second part of the project is more open-ended than the
    first. I will give here a module interface, PROB, and a few
    convenience functions, and then ask you to play with it, implement
    a few ideas, and show me how you've used them. We will give you
    a good grade if we think you did something interesting.

    Note in particular that the interface PROB is not set in stone:
    if, in the course of your use of this library, you discover that
    you need additional primitives, please extend the signature to add
    them, and explain (in comments) what they do and how you use
    them. Adding interesting primitives is a good thing -- not at all
    mandatory, but appreciated.
*)

(** {2 Second part: random computations} *)

(** The last module exported a type ['a search] to represent solutions
    to a search problem. This one has a type ['a dist] that
    expressions possible outputs of a random computation. For example,
    you may define a value [flip_coin : bool dist] that represents
    flipping a random coin, and (morally) returns true half of the
    time, false the other.

    The interesting thing with the ['a dist] type is that we can "run"
    the random computation and get not one output, but *all possible
    outputs, with their respective probabilities*. The output of
    [run compare flip_coin] should be something like
    (I'll explain what the "compare" is for later):

      [(true, 0.5); (false, 0.5)]

    A very nice thing with this ['a dist] is that it allows us to
    check whether some algorithms are "really random", in the sense
    that all their outputs have an equal chance of happening (it's an
    uniform distribution), or if the randomness is skewed (e.g., the
    coin returns true more often than false). For example, if [a] and
    [b] are two integers choosen at random in [0..3], is [a+b]
    uniform, or do some sums happen more often than others? Is [4*a+b]
    uniform? Let's check:

      # let pair = prod (rand 3) (rand 3);;
      val pair : (int * int) Prob.dist = <abstr>
      # let sum = map (fun (a,b) -> a+b) pair;;
      val sum : int Prob.dist = <abstr>
      # let sum4 = map (fun (a,b)-> 4 * a + b) pair;;
      val sum4 : int Prob.dist = <abstr>
      # run compare sum;;
      - : (int * float) list =
      [(0, 0.111111111111111105); (1, 0.22222222222222221);
       (2, 0.333333333333333315); (3, 0.22222222222222221);
       (4, 0.111111111111111105)]
      # run compare sum4;;
      - : (int * float) list =
      [(0, 0.111111111111111105); (1, 0.111111111111111105);
       (2, 0.111111111111111105); (4, 0.111111111111111105);
       (5, 0.111111111111111105); (6, 0.111111111111111105);
       (8, 0.111111111111111105); (9, 0.111111111111111105);
       (10, 0.111111111111111105)]

    From this experiment you can directly see (in fact we have
    _proved_) that [a+b] is not a uniform random number, while [4*a+b]
    is. This is something you should already know: when you throw two
    dice in [1..6] and sum them, the "number in the middle", 7, appear
    more often than any other; same thing for 2 in this example.
*)

module type PROB =
  sig
    type 'a dist

    (** You may (or may not) need to sort values of type ['a] in your
        implementation of the PROB signature. In order to make life
        easier for those of you that decide to do so, I decided than
        [run] would always require a comparison function of type ['a ->
        'a -> int], just as those required by [Map.Make] and
        [Set.Make]. In most cases you can just pass the standard
        function [compare] of OCaml, which has the right type and does
        the right thing for values that don't contain functions.
    *)
    val run : ('a -> 'a -> int) -> 'a dist -> ('a * float) list

    val return : 'a -> 'a dist

    (** an element chosen at random, with equal probability, in the
        given array; you can assume that the array elements are all distinct *)
    val uniform : 'a array -> 'a dist

    (** [rand n] is a random number between 0 and (n-1) *)
    val rand : int -> int dist

    (** be careful to compute the probabilities of the product
        correctly *)
    val prod : 'a dist -> 'b dist -> ('a * 'b) dist
    val map : ('a -> 'b) -> 'a dist -> 'b dist

    val bind : 'a dist -> ('a -> 'b dist) -> 'b dist
  end

(* see http://caml.inria.fr/pub/ml-archives/caml-list/
         2004/09/e3d59b55a2e7ec4359060fd31d23a452.en.html *)
let cons h t = h::t
let list_of_array a = Array.fold_right cons a []

module Prob : PROB =
  struct
    type 'a dist = Dist of 'a list

    let run = fun cmp (Dist l) ->
      let len = float_of_int (List.length l)
      and ll = List.sort cmp l in
        let rec count cur lst cur_count counts =
          match lst with
          | el::lst' when el=cur ->
              count cur lst' (cur_count +. 1.0) counts
          | el::lst' ->
              count el lst' 1.0 ((cur, cur_count /. len)::counts)
          | [] ->
              ((cur, cur_count /. len)::counts)
        in
          match ll with
          | el::ll' ->
              (* sorting is not required here, but this make results
               * easier to read and unit tests easier to write *)
              List.sort compare (count el ll' 1.0 [])
          | [] -> []

    let return x =
      Dist([x])

    let uniform xs =
      Dist(list_of_array xs)

    let rand n =
      let rec rand_a n l =
        if n < 0 then l
        else rand_a (n-1) (n::l)
      in
        Dist(rand_a (n-1) [])

    let prod = fun (Dist d1) (Dist d2) ->
      let rec prod_helper d1 d2 p =
        match d1, d2 with
        | [], _ -> p
        | _, [] -> p
        | x::d1', d2' ->
            let rec prod_on_list el lst p =
              match lst with
              | [] -> prod_helper d1' d2' p
              | y::lst' ->
                  prod_on_list el lst' ((x,y)::p)
            in
              prod_on_list x d2' p
      in
        Dist(prod_helper d1 d2 [])

    let map = fun f (Dist l) ->
      Dist(List.map f l)

    let bind = fun (Dist l) f ->
      let ls =
        (* [rev_map] is more efficient than [map] and we don't care
         * about the order of results *)
        (List.rev_map (fun x -> let Dist(res) = f x in res) l)
      in
        (* optimized version of [Dist(List.concat ls)] which doesn't preserve
         * the order of elements *)
        match ls with
        | [] -> Dist([])
        | h::t ->
            Dist(List.fold_left (fun acc el -> List.rev_append el acc) h t)
  end

(** {2 Where you should go from here} *)

(* I want you to experiment with the Prob modules (or variants of the
   Prob modules if you find extensions or different approaches that
   would be interesting) to study the probabilities of outputs of
   programs using randomness.

*)

(*  
   As a first step, I suggest you study *array shuffles* (ways to
   shuffle randomly the elements of an array); for example, starting
   from the array [|3;4|], you want the shuffle to randomly return
   [|3;4|] or [|4;3|], each with the same probability. There are many
   algorithms to shuffle an array, and most of them are wrong, in the
   sense than some outputs are more likely than the others.

   I recommend that you try to implement, using the [Prob] module, the
   two following algorithms to shuffle an array of size N:

   - algorithm 1: N times, pick two integers i,j among [0..N-1], and
   swap the values of the array at indices i and j

   - algorithm 2: assign to each value of the array a random "weight"
   in [0..N-1], sort the values by their weights, and return the
   corresponding array

   Do those techniques return a shuffled array with uniform
   randomness, or do they return some outputs more often than the
   other?

   Could you write a third shuffling algorithm that is different from
   those two, and is uniform -- each output has equal chances of being
   chosen? I don't mind if you search for an algorithm to do that on
   the internet -- there is a simple, well-known way to do this, that
   is interesting to search for if you have some time, but is not the
   topic of this project.

   There is code below that will help you work with (persistent)
   arrays, and write code using the Prob module that look a bit like
   the imperative algorithm (you cannot directly use OCaml's "for"
   loops to produce ['a dist] values).
*)

(* 
   What to do next?

   
   It's your choice, this project is open-ended. If you stop here, and
   have done correctly the ['a search] part, the ['a dist] module and
   the three shuffles, I will already be happy -- and you may be tired.

   I expect some of you to go further, however. I'm sure there are
   other random algorithms you're interested in, that you could try to
   express with ['a dist] type, to check whether they're uniform or
   what their output probabilities are.

   Alternatively, there are other ways to represent random
   computations. Instead of returning the probability distribution of
   all the outputs, you can define a datatype that will return the
   expected value ("espérance") of the random process. Or only compute
   *some* of the possible outputs, by simulating the algorithm, and
   approximate the real distributions by making several runs. This is
   probably harder than expressing other algorithms, though, so
   I wouldn't recommend it.
*)

(** {2 Helper functions for array-manipulating code} *)

(** The code below will help you write functions doing random
    manipulations on arrays; it is not very interesting so I'm giving it
    directly. *)

(** Side-effects would not mix very well with the various structures
    we have in the Prob module. We will therefore use *persistent*
    arrays, such as the "get" and "set" operation do not mutate an
    existing array in place, but return a new array, without modifying
    the old one.

    To make things simple, I just reused the Map structure of OCaml,
    that provides association maps with keys at any type with
    a comparison function. I define PArray (persistent arrays) as maps
    whose keys are integers. An invariant such maps must respect is
    their keys are exactly 0,1,...,N-1, where N is the size of the
    map.
*)
module PArray = Map.Make(struct
  type t = int
  let compare = compare
end)

(** conversion functions *)
let parray_of_array arr =
  let parray = ref PArray.empty in
  for i = 0 to Array.length arr - 1 do
    parray := PArray.add i arr.(i) !parray
  done;
  !parray

(* Note: PArray.cardinal doesn't seem to be tail-recursive, so using
 * array_of_parray to a get pretty printing sometimes lead to a stack overflow
 *)
let array_of_parray parray =
  Array.init (PArray.cardinal parray)
    (fun i -> PArray.find i parray)

(**
 * Instead of using:
 *  run compare (function_on_parrays |> Prob.map array_of_parray);;
 * Use:
 *  array_probs_of_parray_probs (run compare (function_on_parrays));;
 *
 * This is more efficient and avoid stack overflows.
 **)
(* FIXME: Error: The type of this expression,
 *          ('_a PArray.t * '_b) list -> ('_a array * '_b) list,
 *        contains type variables that cannot be generalized
 *)
let array_probs_of_parray_probs =
  List.map (fun (a, p) -> ((array_of_parray a), p))

(** length of a persistent array *)
let length = PArray.cardinal

(** swapping two indices of a persistent array *)
let swap i j parray =
  parray
  |> PArray.add i (PArray.find j parray)
  |> PArray.add j (PArray.find i parray)


(** Remark: feel free, of course, to add additional convenience
    functions, according to the needs of the code you'll develop. *)

open Prob

(** looping functions; you may want to look at their type *)

let rec for_to i n init f =
  if i > n then return init
  else
    bind (f i init) (fun next ->
      for_to (i + 1) n next f)

let rec for_downto i n init f =
  if i < n then return init
  else
    bind (f i init) (fun next ->
      for_downto (i - 1) n next f)

(**
 * Algorithm 1: N times, pick two integers i,j among [0..N-1], and
 * swap the values of the array at indices i and j
 *
 * Use it like this to get pretty printing:
 *   # array_probs_of_parray_probs (run compare (shuf1 your_array n));;
 *
 * From my tests, this doesn't shuffle uniformly because the initial order is
 * the most likely to be "generated".
 *
 * I could test with N = 1, 2, 3, and 4. N=5 took too much time.
 **)
let shuf1 a =
    let n = Array.length a
    and init = parray_of_array a
    in
      for_to 0 n init (fun _ parr ->
        let r = (rand n) in
          bind (prod r r) (fun (i, j) ->
            return (swap i j parr)))

(**
 * Algorithm 2: assign to each value of the array a random "weight"
 * in [0..N-1], sort the values by their weights, and return the
 * corresponding array
 *
 * Use it like this to get pretty printing:
 *   # array_probs_of_parray_probs (run compare (shuf2 your_array n));;
 *
 **)
let shuf2 a =
  let n = Array.length a
  in
    (* TODO *)
    return a

(**
 * Algorithm 3: Fisher-Yates, modern version as described here:
 *  https://en.wikipedia.org/wiki/Fisher-Yates_shuffle#The_modern_algorithm
 *
 * Use it like this to get pretty printing:
 *   # array_probs_of_parray_probs (run compare (fisher_yates your_array n));;
 *
 * As we already know, this algorithm is uniform, each possible output is as
 * likely to happen than another. It's also significantly more efficient than
 * [shuf1] (TODO compare to shuf2), I was able to run it with arrays of up to 9
 * elements.
 **)
let fisher_yates a =
  let init = parray_of_array a
  and n    = Array.length a
  in
    for_downto (n - 1) 1 init (fun i parr ->
      bind (rand (i+1)) (fun j ->
        return (swap i j parr)))

