(*
                         CS 51 Problem Set 3
                           Bignums and RSA
*)

(*======================================================================
In this problem set, as with the previous one, you may express your
solution to a particular problem in terms of another function from a
previous problem. Furthermore, you may use functions from the List
module where appropriate.
......................................................................*)

(* bignum: Type for representing bignums (arbitrary precision
   integers. Uses a boolean field neg for negative numbers and a list
   coeffs of coefficients from most to least significant. *)
   
type bignum = {neg: bool; coeffs: int list} ;;

(* cBASE: Global constant, the base for representing bignums, which
   must be a power of 10. Your code should work for any reasonable
   value of cBASE, not just the initial value we've set it to. When
   submitting, have cBASE be 1000. *)
let cBASE = 1000 ;;
  
(*......................................................................
Problem 1: Negation
......................................................................*)
  
let negate (b : bignum) : bignum =
  match b with
  | {neg = bl; coeffs = []} -> b
  | {neg = bl; coeffs = n} -> {neg = not bl ; coeffs = n} ;;

(*......................................................................
Problem 2: Comparing bignums
......................................................................*)  
  
let equal (b1 : bignum) (b2 : bignum) : bool =
  match b1, b2 with
  | {neg = bl ; coeffs = n}, {neg = bl2 ; coeffs = n2} -> (bl = bl2) && (n = n2) ;;

let less (b1 : bignum) (b2 : bignum) : bool =
  if b1.neg <> b2.neg then b1.neg 
  else let (l1, l2) = (List.length b1.coeffs, List.length b2.coeffs) in
    if b1.neg = false then 
      if l1 < l2 then true 
      else if l1 = l2 then b1.coeffs < b2.coeffs
      else false
    else 
      if l1 > l2 then true 
      else if l1 = l2 then b1.coeffs > b2.coeffs
      else false ;;


(*Less checks if b1 < b2, so I can just call less in greater but
reverse the two numbers to check for greater
*)
let greater (b1 : bignum) (b2 : bignum) : bool =
  less b2 b1 ;;

(*......................................................................
Problem 3: Converting to and from bignums
......................................................................*)
let rec from_int_helper (n : int) : int list =
  match n with
  | 0 -> []
  | _ -> from_int_helper (n/cBASE) @ [abs (n mod cBASE)]

let from_int (n : int) : bignum =
  if n < 0 then {neg = true ; coeffs = from_int_helper n}
  else {neg = false ; coeffs = from_int_helper n} ;;

let to_int (b : bignum) : int option =
  if less (from_int max_int) b || less b (from_int min_int) then None
  else 
    let rec to_int_help (lst : int list) : int =
      match lst with
      | [] -> 0
      | hd::tl -> hd + cBASE*(to_int_help tl)
    in 
    if b.neg then Some ((~-) (to_int_help (List.rev b.coeffs)))
    else Some (to_int_help (List.rev b.coeffs)) ;;


(*......................................................................
Helpful functions (not to be used in problems 1 to 3)
......................................................................*)

(* strip_zeroes -- Removes zero coefficients from the beginning of the
   coefficients part of a bignum representation *)
let rec strip_zeroes (b : int list) : int list =
  match b with
  | 0 :: t -> strip_zeroes t
  | _ -> b ;;

(* clean -- Removes zero coefficients from the beginning of a bignum
   representation *)
let clean (b : bignum) : bignum =
  {neg = b.neg; coeffs = strip_zeroes b.coeffs} ;;

(* rand_bignum -- Returns a random bignum from 0 to bound (inclusive).
   You might use this to help randomly test functions. *)
let rand_bignum (bound: bignum) : bignum =
  let randbase = List.map (fun _ -> Random.int cBASE) in
  let rec rand_bignum_rec (bound: int list) =
    match bound with
    | [] -> []
    | h::t -> let r = Random.int (h+1) in
              r::((if r = h then rand_bignum_rec else randbase) t)
  in {neg = false; coeffs = strip_zeroes (rand_bignum_rec bound.coeffs)} ;;
       
(* explode -- Splits a string into a list of its characters. *)
let rec explode (s : string) : char list =
  let len = String.length s in
  if len = 0 then []
  else s.[0] :: explode (String.sub s 1 (len - 1)) ;;

(* implode -- Condenses a list of characters into a string. *)
let rec implode (cs : char list) : string =
  match cs with
  | [] -> ""
  | c :: t -> String.make 1 c ^ implode t ;;
                                          
(* take_first -- Returns the first n elements of list l (or the whole
   list if too short) *)
let rec take_first (l : 'a list) (n : int) : 'a list =
  match l with
  | [] -> []
  | h :: t -> if n <= 0 then [] else h :: take_first t (n - 1) ;;

(* split -- Returns a pair (first n elements of lst, rest of elements
   of lst) *)
let rec split lst n =
  if n = 0 then ([], lst)
  else match lst with
  | [] -> ([], [])
  | h :: t -> let (lst1, lst2) = split t (n - 1) in
              (h :: lst1, lst2) ;;

(* intlog -- Returns the floor of the base 10 log of an integer *)
let intlog (base : int) : int =
  int_of_float (log10 (float_of_int base)) ;;

(* from_string -- Converts a string representing an integer to a
   bignum. Assumes the base is a power of 10. *)
let from_string (s : string) : bignum =
  let rec from_string_rec (cs : char list) : int list =
    if cs = [] then [] else
    let (chars_to_convert, rest) = split cs (intlog cBASE) in
    let string_to_convert = implode (List.rev chars_to_convert) in
    int_of_string string_to_convert :: from_string_rec rest
  in
  match explode s with
  | [] -> from_int 0
  | h :: t ->
      if h = '-' || h = '~' then
        {neg = true; coeffs = (List.rev (from_string_rec (List.rev t)))}
      else {neg = false;
            coeffs =
              (strip_zeroes (List.rev (from_string_rec (List.rev (h :: t)))))}

(* to_string -- Converts a bignum to its string representation.
   Returns a string beginning with ~ for negative integers. Assumes
   the base is a power of 10. *)
let to_string (b : bignum) : string =
  let rec pad_with_zeroes_left (s : string) (len : int) =
    if String.length s >= len then s
    else "0" ^ pad_with_zeroes_left s (len - 1) in
  let rec stripstrzeroes (s : string) (c : char) =
    if String.length s = 0 then
      "0"
    else if String.get s 0 = '0' then
      stripstrzeroes (String.sub s 1 (String.length s - 1)) c
    else s in
  let rec coeffs_to_string (coeffs : int list) : string =
    match coeffs with
    | [] -> ""
    | h :: t -> pad_with_zeroes_left (string_of_int h) (intlog cBASE)
                ^ coeffs_to_string t in
  let stripped = strip_zeroes b.coeffs in
  if List.length stripped = 0 then "0"
  else let from_coeffs = stripstrzeroes (coeffs_to_string stripped) '0' in
       if b.neg then "~" ^ from_coeffs else from_coeffs ;;

(*......................................................................
Arithmetic functions
......................................................................*)

(* plus_pos -- Returns a bignum representing b1 + b2.  Assumes that the
   sum is positive. *)
let plus_pos (b1 : bignum) (b2 : bignum) : bignum =
  let pair_from_carry (carry : int) =
    if carry = 0 then (false, [])
    else if carry = 1 then (false, [1])
    else (true, [1])
  in
  let rec plus_with_carry (neg1, coeffs1) (neg2, coeffs2) (carry : int)
    : bool * int list =
    match (coeffs1, coeffs2) with
    | ([], []) -> pair_from_carry carry
    | ([], _) ->
        if carry = 0 then (neg2, coeffs2)
        else plus_with_carry (neg2, coeffs2) (pair_from_carry carry) 0
    | (_, []) ->
        if carry = 0 then (neg1, coeffs1)
        else plus_with_carry (neg1, coeffs1) (pair_from_carry carry) 0
    | (h1 :: t1, h2 :: t2) ->
        let (sign1, sign2) =
            ((if neg1 then -1 else 1), (if neg2 then -1 else 1)) in
        let result = h1 * sign1 + h2 * sign2 + carry in
        if result < 0 then
          let (negres, coeffsres) =
              plus_with_carry (neg1, t1) (neg2, t2) (-1)
          in (negres, result + cBASE :: coeffsres)
        else if result >= cBASE then
          let (negres, coeffsres) = plus_with_carry (neg1, t1) (neg2, t2) 1
          in (negres, result - cBASE :: coeffsres)
        else
          let (negres, coeffsres) = plus_with_carry (neg1, t1) (neg2, t2) 0
          in (negres, result :: coeffsres)
  in
  let (negres, coeffsres) =
      plus_with_carry (b1.neg, List.rev b1.coeffs)
                      (b2.neg, List.rev b2.coeffs)
                      0
  in {neg = negres; coeffs = strip_zeroes (List.rev coeffsres)} ;;

(*......................................................................
Problem 4

The plus function returns a bignum representing b1 + b2. However,
it does NOT make the same assumption as plus_pos. 

Hint: How can you use plus_pos to implement plus? Make sure that
your implementation preserves the bignum invariant.
......................................................................*)

let plus (b1 : bignum) (b2 : bignum) : bignum =
  if b1.neg && b2.neg then negate (plus_pos (negate b1) (negate b2))
  else if not b1.neg && not b2.neg then (plus_pos b1 b2)
  else if b1.neg && not b2.neg then 
    if less (negate b1) b2 then (plus_pos b1 b2)
    else negate (plus_pos (negate b1) (negate b2))
  else 
    if greater b1 (negate b2) then (plus_pos b1 b2)
    else negate (plus_pos (negate b1) (negate b2)) ;;

(*......................................................................
Problem 5

The times function returns a bignum representing b1 * b2. Think about
how you were first taught multiplication:

      543 
    x 224
    -----
     2172
  + 10860 <--- Note that a zero is appended after the partial product
 + 108600 <--- Note that two zeroes are appended after the partial product
 --------
 = 121632  

When approaching this problem, it is advisable to break the problem
down into simpler, easier-to-implement sub-problems. That way, if a
bug arises in your code, you can test each helper function
individually rather than having to test all of it at once.

You may assume positivity in some of your helper functions if it 
simplifies the code, as long as the invariant is preserved. 
......................................................................*)

let times (b1 : bignum) (b2 : bignum) : bignum =
  failwith "times not implemented" ;;

(*======================================================================
Challenge Problem 8: Faster bignum multiplication 

The function "times_faster" returns a bignum representing b1 * b2. 
......................................................................*)

let times_faster (b1 : bignum) (b2 : bignum) : bignum =
  failwith "times_faster not implemented" ;;

(*======================================================================
Time estimate

Please give us an honest (if approximate) estimate of how long (in
minutes) this part of the problem set took you to complete.  We care
about your responses and will use them to help guide us in creating
future assignments.
......................................................................*)

let minutes_spent_on_pset () : int = 250 ;;
