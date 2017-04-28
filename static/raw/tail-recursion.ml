(** Let's talk about tail-recursion! *)

(** First: let's build big lists! *)

(**
We want:
range i j = [i; i+1; ...; j-1; j] when i ⩽ j
range i j = []                    when i > j
 *)

let rec range i j =
  if i > j
  then []
  else i :: range (i + 1) j

let _ = range 0 100

(** Works fine! *)

let _ = range 0 1_000_000

(** Oops! Stack overflow during evaluation. Why? *)

(**
   range 0 1_000_000
-> 0 :: range 1 1_000_000
-> 0 :: (1 :: range 2 1_000_000)
-> 0 :: (1 :: (2 :: range 3 1_000_000))
   ^^^^^^^^^^^^^^^^
this "stack" of things to be done after the recursive calls
keeps on growing, until it is too big!
 *)

let range_tailrec_1 i j =
  let rec aux i j range_so_far =
    if i > j
    then range_so_far
    else aux (i + 1) j (range_so_far @ [i])
  in
  aux i j []

(** Now when we run:
   range_tailrec_1 0 2
-> aux 0 2 []
-> aux 1 2 [0]
-> aux 2 2 [0; 1]
-> aux 3 2 [0; 1; 2]
-> [0; 1; 2]

Now, it's the range_so_far argument that keeps on growing,
but arguments are placed in the computer heap, which can
use much more space than the stack is allowed to.

However, this will still not work, because we used (@):
   range_tailrec_1 0 1_000_000
-> aux 0 1_000_000 []
-> ...
-> aux 500_000 1_000_000 [....................]
-> aux 500_001 1_000_000 ([....................] @ [500_000])
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
appending to a list of five hundred thousands elements
requires to go through the entire list, and (@) is not
tail-recursive so the stack still overflows! :(

We must avoid (@) altogether:
 *)

let range_tailrec_2 i j =
  let rec aux i j range_so_far =
    if i > j
    then range_so_far
    else aux 1 (j - 1) (j :: range_so_far)
  in
  aux i j []

(** Now when we run:
   range_tailrec_2 0 1_000_000
-> aux 0 1_000_000                   []
-> aux 0   999_999          [1_000_000]
-> aux 0   999_998 [999_999; 1_000_000]

By building the list from the end, decrementing j, we
only ever use (::) which is the most simple operation
on lists.
 *)

(** Let's try to make another function tail-recursive: *)

let max x y = if x > y then x else y

(** We want to compute the maximum of a list of
    positive integers: *)

(** Not tail-recursive: *)
let rec list_max xs =
  match xs with
  | [] -> 0
  | h::t -> max h (list_max t)

(** Tail-recursive: *)
let max_list l =
  let rec helper max_so_far remaining_elmts =
    match remaining_elmts with
    | [] -> max_so_far
    | h::t -> helper (max max_so_far h) t
  in
  helper 0 l

(** Once again, we succeeded by:
    - defining an auxiliary function with an additional
      argument
    - having the recursive calls build the result in the
      extra argument
 *)

(*******************************************************)

(** At the end of the lecture, I briefly showed how
    to define your own "mylist" type, that behaves like
    Ocaml's "list" type: *)

type 'a mylist =
  | Mynil
  | Mycons of 'a * 'a mylist

(** This tells OCaml:
    - define a type called "mylist",
      generic over any type 'a
    - with one constructor called "mycons",
      taking no arguments
    - with one constructor called "mycons",
      taking a pair of 2 arguments
      (one is an 'a, one is an 'a mylist)

We can then build some "mylist"s:
 *)

(**   This is like (3 :: (2 :: []))   *)
let l1 = Mycons (3, Mycons(2, Mynil))

let rec mylength ml =
  match ml with
  | Mynil        -> 0
  | Mycons(h, t) -> 1 + mylength t
