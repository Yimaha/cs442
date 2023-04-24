open Base;;
open Stdio;;

let push lst v = v::lst;;

let binary lst func = 
  match lst with    
  | first::second::rest -> Some((func first second), (func first second)::rest)
  | _ -> None;;

let add lst = 
  let adder x y = x +. y in
  binary lst adder

let sub lst = 
  let adder x y = y -. x in
  binary lst adder

let mul lst = 
  let adder x y = x *. y in
  binary lst adder
      
let div lst = 
  let adder x y = y /. x in
  binary lst adder

let print lst =
  printf ("List (");
  List.iter lst ~f:(printf "%F ");
  printf (")");;

let list = [2.;3.;4.;5.;6.];;

let list = push list 3.
let list = push list 40.

let Some(result, list) = add list;;
let Some(result, list) = mul list;;
let Some(result, list) = sub list;;

print list

(* print result *)