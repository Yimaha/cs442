open Core
open Core.Poly
open Conlog


let rec unification_s_expr(l: string) (r: expr): (substitution list) option = 
  match r with
  | Primary prim -> (
    if ((isNum l) && (isNum prim) && (String.equal l prim)) then (Some([]))
    else if ((isAtom l) && (isAtom prim) && (String.equal l prim)) then (Some([]))
    else if (isVarName l) then (Some([(l, prim)]))
    else (None)
  )
  | Binexp (left, op, right) -> (
    match (solve(r)) with
    | None -> None
    | Some(r_val) -> (
      if ((isNum l) && (int_of_string l = r_val)) then (Some([]))
      else if (isVarName l) then (Some([(l, (string_of_int r_val))]))
      else (None)
    )
  )

let rec unification_p_h_prime_recur (p_body : string list) (h_prime_body : string list): (substitution list) option =
  match (p_body, h_prime_body) with
  | (p_b::rest_p, h_b::rest_h) -> (
    if (isVarName p_b) then (
      let s = (p_b, h_b) in
      match (unification_p_h_prime_recur (subArgs rest_p [s]) (subArgs rest_h [s])) with
      | None -> None
      | Some( back ) -> Some(s :: back)
    ) else (
      if isVarName h_b then (
        let s = (h_b, p_b) in
        match (unification_p_h_prime_recur (subArgs rest_p [s]) (subArgs rest_h [s])) with
        | None -> None
        | Some( back ) -> Some(s :: back)
      ) else (
         (*check if they are equal, if they are not equal return none*)
          match String.equal p_b h_b with
          | true -> (unification_p_h_prime_recur rest_p rest_h)
          | false -> None
      )
    )
  )
  | ([], []) -> Some([])
  | _ -> None

let rec unification_p_h_prime ((p_head, p_body): relation) ((h_prime_head, h_prime_body): relation): (substitution list) option =
  match (String.equal p_head h_prime_head) with 
  | false -> (None)
  | true -> (
    match (List.length p_body) = (List.length h_prime_body) with
    | false -> (None)
    | true -> (
      unification_p_h_prime_recur p_body h_prime_body
    )
  )

let rec query (db : database) (q : predicate list) : (substitution list) option =
  (* printPredicateList q;
  printf "\n"; *)
  match q with 
  (* the query is satisfied, return empty substitution*)
  | [] -> (Some([]))
  | p::q_prime -> (
    match p with
    | Relation rel -> (
      let rec tryQuery = (function 
      | clause :: cl -> (
        let (head_prime, body_prime) = freshenHornClause clause in
        let s = unification_p_h_prime rel head_prime in 
        match s with
        | None -> (
          tryQuery cl
        )
        | Some(sub) -> (

          let q_prime_prime = subPredicateList (List.append body_prime q_prime) sub in
          let s_prime = query db q_prime_prime in
          match s_prime with 
          | None -> (
            tryQuery cl
          )
          | Some(sub_prime) -> (
            (* printSubstitutions sub;
            printSubstitutions sub_prime; *)
            Some(List.append sub sub_prime)
          )
        )
      )
      | _ -> None) in
      tryQuery db
    )
    | Comparison (left, op, right) -> (
      match op with
      | "is" -> (
        let s = unification_s_expr left right in
        match s with
        | None -> None
        | Some(sub) -> (
          match (query db (subPredicateList q_prime sub)) with 
          | None -> None
          | Some(remain_sub) -> Some(List.append sub remain_sub)
        ) 
      )
      | _ ->  (
        match (isNum left) with 
        | false -> None
        | true -> (match solve(right) with 
          | None -> None
          | Some(r) -> (
            let l = int_of_string left in
            match op with 
            | ">" -> (
              match l > r with 
              | false -> None
              | true -> query db q_prime
            )
            | ">=" -> (
              match l >= r with 
              | false -> None
              | true -> query db q_prime
            )
            | "<" -> (
              match l < r with 
              | false -> None
              | true -> query db q_prime
            )
            | "<=" -> (
              match l <= r with 
              | false -> None
              | true -> query db q_prime
            )
            | "=\\=" -> (
              match l <> r with 
              | false -> None
              | true -> query db q_prime
            )
            | _ -> None
          )
        )
      ) 
    )
  )


