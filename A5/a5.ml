
open Core
open Core.Poly
open Base
open SIL

type terminal = 
| NumTerm of int
| Proc of procedure
| Label of string
| Arr of int array

type storetype = ((string, terminal) Base.Hashtbl.t)

let rec run (l : statement list) = (
    step l (Hashtbl.create (module String))
)

and step (l :statement list) (store: storetype)= (
    match l with 
    | [] -> ()
    | s::rest -> (
      let rec resolveStatement = (function
        | Block (bsl) -> (
          step bsl store
        )
        | WhileStmt (b, q) -> (
          match (resolveBool b store) with
          | true -> (resolveStatement q); (resolveStatement (WhileStmt(b, q)));
          | false -> () (* skip *)
        )
        | IfStmt (b, q1, q2) -> (
          match (resolveBool b store) with
          | true -> (resolveStatement q1)
          | false -> (resolveStatement q2)
        )
        | VarAssgStmt (x, m) -> (
          assignVal x (resolveExpr m store) store
        ) 
        | PrintStmt (e) -> printf "%d\n" (resolveExprInt e store)
        | ProcDecl (funcName, params, decls, body) -> assignVal funcName (Proc(funcName,params,decls,body)) store
        | CallStmt (funcName, args) -> (prodCall funcName (callArgs args store) store)
        | NewArrStmt (arrayName, sz) -> (assignVal arrayName (Arr(Array.create ~len:(resolveExprInt sz store) 0)) store)
        | ArrAssgStmt (arrayName, index, value) -> (arrAssign arrayName (resolveExprInt index store) (resolveExprInt value store) store)
        | Skip -> ()
      ) in
      resolveStatement s;
      step rest store;
    )
)

and resolveBool (b: boolExpr) (store: storetype): bool = (
  match b with
  | True -> true
  | False -> false
  | Not (b1) -> not (resolveBool b1 store)
  | And (b1, b2) -> (resolveBool b1 store) && (resolveBool b2 store)
  | Or (b1, b2) -> (resolveBool b1 store) || (resolveBool b2 store)
  | Gt (e1, e2) -> (resolveExprInt e1 store) > (resolveExprInt e2 store)
  | Lt (e1, e2) -> (resolveExprInt e1 store) < (resolveExprInt e2 store)
  | Eq (e1, e2) -> (resolveExprInt e1 store) = (resolveExprInt e2 store)
)



and resolveExpr (e: expr) (store: storetype): terminal = (
  match e with
  | Num (i) -> NumTerm(i)
  | Var (s) -> getVal s store
  | AddExpr (e1, e2) -> NumTerm((resolveExprInt e1 store) + (resolveExprInt e2 store))
  | MulExpr (e1, e2) -> NumTerm((resolveExprInt e1 store) * (resolveExprInt e2 store))
  | NegExpr (e1) -> NumTerm(-(resolveExprInt e1 store))
  | ArrIndexExpr (e1, e2) -> (
    match resolveExpr e1 store with
    | Arr (arr) -> NumTerm(Array.get arr (resolveExprInt e2 store))
    | Label (l) -> (
      match (traceToArray l store) with 
      | Some(arr) -> NumTerm(Array.get arr (resolveExprInt e2 store))
      | None -> (NumTerm(0))
    )
    | _ -> (NumTerm(0)) (*default bahaviour is to return nothing*)
  )
)

and resolveExprInt (e: expr) (store: storetype): int = (
  match (resolveExpr e store) with 
  | NumTerm (n) -> n 
  | _ -> 0
)

and assignVal (k: string) (n: terminal) (store: storetype) = (
  match n with 
  | NumTerm (num) ->  Hashtbl.set store ~key: k ~data: n
  | Proc (p) -> Hashtbl.set store ~key: k ~data: n
  | Label (l) -> (
    match (getVal l store) with 
    | NumTerm (num) -> (Hashtbl.set store ~key: k ~data: (NumTerm(num)))
    | Proc (p) -> Hashtbl.set store ~key: k ~data: (Proc(p))
    | Label (lab) -> Hashtbl.set store ~key: k ~data: (Label(lab)) (*chain of labels if needed*)
    | Arr (arr) -> Hashtbl.set store ~key: k ~data: (Label(l)) (*create a label that point toward the arrays*)
  )
  | Arr (arr) -> Hashtbl.set store ~key: k ~data: n
)

and getVal (k: string) (store: storetype): (terminal) = (
  match(Hashtbl.find store k) with
  | Some(v) -> v
  | None -> NumTerm(0)
)

and callArgs (args: expr list) (store: storetype): (terminal list) = (
  match args with 
  | e::rest -> (resolveExpr e store)::(callArgs rest store)
  | [] -> []
)

and prodCall (k: string) (args: terminal list) (store: storetype) = (
  match(Hashtbl.find store k) with
  | None-> ()
  | Some(Proc(x, a, d, body)) -> (
      let (freshenArgs, freshenStatements) = (freshenProcedure a d body) in 
      let rec resolveFuncCreatVar = (function
        | (argName::nameRest, arg::argRest) -> (
          assignVal argName arg store;
          resolveFuncCreatVar (nameRest, argRest);)
        | _ -> ()
      ) in 
      resolveFuncCreatVar (freshenArgs, args);
      step freshenStatements store
  )
  | _ -> ()
)

and traceToArray (k: string) (store: storetype) = (
  match getVal k store with 
  | Label(l) -> traceToArray l store
  | Arr(arr) -> Some(arr)
  | _ -> None
)

and arrAssign (k: string) (index: int) (value: int) (store: storetype) = (
  match traceToArray k store with 
  | None -> ()
  | Some(arr) -> Array.set arr index value
)