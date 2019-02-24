(* 
			 CS 51 Problem Set 3 -- Testing
 *)

open Bignum ;;

open Simple ;;      (* a really simple unit testing framework *)
  
let test () =
  unit_test ((negate {neg = false; coeffs = []}) = {neg = false; coeffs = []}) "negate empty";
  unit_test ((negate {neg = false; coeffs = [111;222]}) = {neg = true; coeffs = [111;222]}) "negate positive";
  unit_test ((negate {neg = true; coeffs = [111;222]}) = {neg = false; coeffs = [111;222]}) "negate negative";

  unit_test ((equal {neg = false; coeffs = [111;222]} {neg = false; coeffs = [111;222]}) = true) "equal";
  unit_test ((equal {neg = true; coeffs = [111;222]} {neg = false; coeffs = [111;222]}) = false) "not equal, different signs";
  unit_test ((equal {neg = true; coeffs = [111;222]} {neg = true; coeffs = [111;222;333]}) = false) "not equal, different coeffs";

  unit_test ((less {neg = true; coeffs = [111;222]} {neg = false; coeffs = [111;222]}) = true) "different signs less";
  unit_test ((less {neg = true; coeffs = [111;222]} {neg = true; coeffs = [111;222;333]}) = false) "negatives less";
  unit_test ((less {neg = false; coeffs = [111;222]} {neg = false; coeffs = [111;222;333]}) = true) "positives less";

  unit_test ((greater {neg = true; coeffs = [111;222]} {neg = false; coeffs = [111;222]}) = false) "different signs greater";
  unit_test ((greater {neg = true; coeffs = [111;222]} {neg = true; coeffs = [111;222;333]}) = true) "negatives comparison greater";
  unit_test ((greater {neg = false; coeffs = [111;222]} {neg = false; coeffs = [111;222;333]}) = false) "positives comparison greater";  

  unit_test ((from_int 0) = {neg = false; coeffs = []}) "from_int 0";
  unit_test ((from_int 111222) = {neg = false; coeffs = [111;222]}) "positives";
  unit_test ((from_int ~-111222) = {neg = true; coeffs = [111;222]}) "negative";
  unit_test ((from_int max_int) = {neg = false; coeffs = [1; 222; 333; 444; 555; 666; 777]}) "from_int max_int";    
  unit_test ((from_int min_int) = {neg = true; coeffs = [1; 222; 333; 444; 555; 666; 777]}) "from_int min_int";

  unit_test ((to_int {neg = false; coeffs = []}) = Some 0) "to_int 0";
  unit_test ((to_int {neg = false; coeffs = [111; 222]}) = Some 111222) "positive";
  unit_test ((to_int {neg = true; coeffs = [111; 222]}) = Some ~-111222) "negative";
  unit_test ((to_int {neg = false; coeffs = [1; 222; 333; 444; 555; 666; 777]}) = None) "too big";
  unit_test ((to_int {neg = true; coeffs = [1; 222; 333; 444; 555; 666; 777]}) = None) "too small";

  unit_test ((plus {neg = true; coeffs = [111;222]} {neg = false; coeffs = [1;0]}) =  {neg = true; coeffs = [110;222]}) "plus different signs negative";
  unit_test ((plus {neg = false; coeffs = [111;222]} {neg = true; coeffs = [1;0]}) =  {neg = false; coeffs = [110;222]}) "plus different signs positive";
  unit_test ((plus {neg = true; coeffs = [111;222]} {neg = true; coeffs = [1;0]}) =  {neg = true; coeffs = [112;222]}) "plus same signs negative";
  unit_test ((plus {neg = false; coeffs = [111;222]} {neg = false; coeffs = [1;0]}) =  {neg = false; coeffs = [112;222]}) "plus same signs positive";

  () ;;

test ();;
