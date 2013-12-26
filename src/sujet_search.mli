(** {1 Basic definitions} *)

type ('a, 'b) sum = Left of 'a | Right of 'b
(** An element of [('a, 'b) sum] is either a ['a] or a ['b] *)

type 'a stream = Stream of (unit -> ('a * 'a stream) option)
(** (possibly infinite) streams of values *)

(** you have to implement the functions below; this may not be very
    useful for the rest of the project, but consider this a warmup
    exercise *)
val nil : 'a stream
val cons : 'a -> 'a stream -> 'a stream

(** lazy cons *)
val lcons : 'a -> (unit -> 'a stream) -> 'a stream

val get : 'a stream -> ('a * 'a stream) option
val take : 'a stream -> int -> 'a list

val number_stream : int stream
(** the infinite stream [0,1,2,3...] *)

val stream_of_list : 'a list -> 'a stream

(** {1 First part: fair search}

    The goal of the module Logic1, whose signature is below, is to
    represent search problems: an OCaml expression of type ['a search]
    represents a description of values of type ['a] that we would like
    to search for. For example, with the combinators of this module
    you can define a value of type [(int * int) search] that searches
    for a couple of prime numbers [(p,q)] such that [q = p+2] or, if
    you want, that represent all such couples.

    In the general case, such search problems may have zero, several
    or even an infinite number of solutions. You will have to
    implement a function
{[
    solve : int -> 'a search -> 'a list
]}

    such that [solve n prob] returns a list of [n] solutions to the
    search problem [prob], or less if there do not exist so enough
    solutions. Solutions may be returned in any order, but you must be
    ale to provide all possible solutions (if there is a finite number
    of solutions).
*)
module Logic1 :
  sig
    type 'a search
    (** the type of search descriptions *)

    val solve : int -> 'a search -> 'a list
    (** [solve n prob] returns [n] solutions to the search problem
        [prob], or less if there aren't so many solutions *)

    val stream : 'a stream -> 'a search
    (** a stream can be seen as a search problems whose solutions are
        all the elements present in the stream *)

    val return : 'a -> 'a search
    (** a single value also defines a search problem (which has
        exactly this value as solution *)

    val fail : 'a search
    (** the search problem with no solution *)

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
  end

(** Evaluation

The following code should work:

{[
let () =
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

  solve 10 pytha |> List.iter
      (fun ((a,b),c) -> Printf.printf "%d² + %d² = %d²\n" a b c) 
]}

    A difficulty with this exercize is that [posnumber] has an
    infinite number of solutions. If you haven't defined [prod]
    carefully enough, this program may loop without returning
    anything, despite having simple solutions.

    PS: (|>) and (@@) are infix operators defined in OCaml 4.01 as
    let (@@) f x = f x
    let (|>) x f = f x

    If you have an older OCaml version, just define them
    at the top of your file.
*)




(** {1 Second part: distribution probabilities}



*)
