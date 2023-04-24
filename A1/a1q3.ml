open Base;;
open Stdio;;

let increment state =
  match state with 
  | (tapeleft, tc, taperight, pc) -> (tapeleft, tc+1, taperight, pc+1)

let decrement state =
  match state with 
  | (tapeleft, tc, taperight, pc) -> (tapeleft, tc-1, taperight, pc+1)
  
let stepLeft state =
  match state with
  | (x :: tapeleft, tc, taperight, pc) -> (tapeleft, x, tc :: taperight, pc+1)
  | ([], tc, taperight, pc) -> ([], 0, tc :: taperight, pc+1);;

let stepRight state =
  match state with
  | (tapeleft, tc, x::taperight, pc) -> (tc::tapeleft, x, taperight, pc+1)
  | (tapeleft, tc, [], pc) -> (tc::tapeleft, 0, [], pc+1);;

let print state =
  let tapeleft, tc, taperight, pc = state in
  match Char.of_int tc with 
  | Some c -> printf "%c" c; (tapeleft, tc, taperight, pc+1)
  | None -> (tapeleft, tc, taperight, pc+1)

let jump state param = 
  match state with
  | (tapeleft, tc, taperight, pc) -> (tapeleft, tc, taperight, pc+param)

let branch state param = 
  match state with
  | (tapeleft, tc, taperight, pc) -> if tc = 0 then (tapeleft, tc, taperight, pc+param) else (tapeleft, tc, taperight, pc+1)
  

let step ins instate = 
  let tapeleft, tc, taperight, pc = instate in
  let length = Array.length ins in 
  if pc = length then None 
  else 
    let (instruction, param) = ins.(pc) in
    match instruction with
    | "+" -> Some(increment instate)
    | "-" -> Some(decrement instate)
    | "<" -> Some(stepLeft instate)
    | ">" -> Some(stepRight instate)
    | "." -> Some(print instate)
    | "J" -> Some(jump instate param)
    | "B" -> Some(branch instate param)
    | _ -> Some(instate);;


let rec runSteps ins inState =
  match step ins inState with
  | Some outState -> runSteps ins outState
  | None -> inState;;

let run prog = 
  ignore (runSteps prog ([], 0, [], 0));;

