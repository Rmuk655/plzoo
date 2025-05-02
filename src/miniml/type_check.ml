(** Type checking. *)

open Syntax

(* This helper function checks if an expression is guaranteed to always raise an exception.

  - If the expression is a 'Raise', then it definitely raises.
  - If it is a 'TryWith' (a try-catch block), then it will only always raise
    if both the try part (e1) and the handler (e2) always raise.
  - Otherwise, it does not always raise.
  This is used during type checking of 'try ... with ...' to avoid needing a specific
  type for expressions that never return normally (i.e., always raise).*)
let rec always_raises expr =
  match expr.Zoo.data with
  | Raise _ -> true
  | TryWith (e1, _, e2) -> always_raises e1 && always_raises e2
  | _ -> false

(* This helper function checks if two types can be made the same (unified).
  - If they are the same basic type (int, bool, or string), it is fine.
  - If they are both function types, it checks their input and output types.
  - If one is an unknown type (TVar), it remembers what type it should be 
  (when inferring the type of the TVar variable from it's value or any other
  additional information, the var with TVar gets bound to a real variable type.
  This real variable type binding to the var is remembered by the computer).
  - If an unknown type already has a value, it checks that value instead.
  - If the types do not match, it gives an error.
  This is used when figuring out the types of expressions.*)
let rec unify ty1 ty2 =
    match ty1, ty2 with
        | TInt, TInt | TBool, TBool | TString, TString -> () (*Same basic types, hence do nothing.*)
        | TArrow (a1, r1), TArrow (a2, r2) -> unify a1 a2; unify r1 r2 (*Same function types are unified.
        TArrow(a1, r1) means the function has an input of type a1, output of type r1.*)
        | TVar ({contents = None} as r), ty 
        | ty, TVar ({contents = None} as r) -> r := Some ty (*If either of the two types are of type TVar 
      with no value (meaning, we cant figure out the type of the variable), then assign the variable with
      type TVar the same type as the type of the second variable, whose type is known.*)
        | TVar {contents = Some ty1}, ty2
        | ty1, TVar {contents = Some ty2} -> unify ty1 ty2 (*If either of the two types are of type TVar 
      with a given value (meaning, we can figure out the type of the variable), then assign the variable with
      type TVar the same type as the type it's value, whose type can is known.*)
        | _ -> failwith "unification failed" (*If all of the above cases fail, we cannot unify the two 
        variables, giving us an error.*)         

        let typing_error ~loc = Zoo.error ~kind:"Type error" ~loc

(** [check ctx ty e] verifies that expression [e] has type [ty] in
    context [ctx]. If it does, it returns unit, otherwise it raises the
    [Type_error] exception. *)
let rec check ctx ty ({Zoo.loc;_} as e) =
  let ty' = type_of ctx e in
(*  if ty' <> ty then *)
(*The above line did not work for exception from within function like:  
let fact = fun f (n : int) : int is
if n < 0 then raise GenericException(-99)
else if n = 0 then 1
else n * f (n - 1) ;; *)
    try unify ty ty' with _ ->
     typing_error ~loc 
        "This expression has type %t but is used as if it has type %t"
        (Print.ty ty')
        (Print.ty ty)

(** [type_of ctx e] computes the type of expression [e] in context
    [ctx]. If [e] does not have a type it raises the [Type_error]
    exception. *)
and type_of ctx {Zoo.data=e; loc} =
  match e with
    | Var x ->
      (try List.assoc x ctx with
	  Not_found -> typing_error ~loc "unknown variable %s" x)
    | Int _ -> TInt
    | Bool _ -> TBool
    | Times (e1, e2) -> check ctx TInt e1 ; check ctx TInt e2 ; TInt
    | String _ -> TString (*Matching the expression with the String Type (TString or Type String)*)
    | Divide (e1, e2) -> check ctx TInt e1 ; check ctx TInt e2 ; TInt
    (*Matching the expression with the division of two expressions e1, e2, while checking that the two expression both 
    evaluate to integers only (as my division implementation only support integer division).*)
    | Plus (e1, e2) -> check ctx TInt e1 ; check ctx TInt e2 ; TInt
    | Minus (e1, e2) -> check ctx TInt e1 ; check ctx TInt e2 ; TInt
    | Equal (e1, e2) -> check ctx TInt e1 ; check ctx TInt e2 ; TBool
    | Less (e1, e2) -> check ctx TInt e1 ; check ctx TInt e2 ; TBool
    | If (e1, e2, e3) ->
      check ctx TBool e1 ;
      let ty = type_of ctx e2 in
	check ctx ty e3 ; ty
| Raise (DivisionByZero) -> TVar (ref None)
| Raise (GenericException _) -> TVar (ref None)
| Syntax.TryWith (e1, _exn, e2) ->
   (* First, we check the type of the handler expression (e2). If the handler expression 
       always raises an exception, we assign it a type variable (TVar ref None), 
       which means it can be any type that will match later. Otherwise, we check its type normally. *)  
      let t2 =
        if always_raises e2 then TVar (ref None)
        else type_of ctx e2
      in
    (* Now, we check the type of the expression inside the 'try' block (e1). 
       If e1 always raises an exception, we set its type to match the type of e2 
       because the exception handler will take over in such cases. Otherwise, 
       we check its type normally. *)      
      let t1 =
        if always_raises e1 then t2
        else type_of ctx e1
      in
      (*Now we unify the types of e1 and e2. 
        This ensures that the type of the try block (e1) is compatible with 
        the type of the handler (e2). In the case of nested 'try' blocks, we need 
        to ensure that the types can match up properly, so they are unified here. *)
      unify t1 t2;
       (* Return the unified type (t1), which is the type of the entire 'try ... with' block. *)
      t1

    | Fun (f, x, ty1, ty2, e) ->
      check ((f, TArrow(ty1,ty2)) :: (x, ty1) :: ctx) ty2 e ;
      TArrow (ty1, ty2) 
    | Apply (e1, e2) ->
      begin match type_of ctx e1 with
	  TArrow (ty1, ty2) -> check ctx ty1 e2 ; ty2
	| ty ->
	  typing_error ~loc
            "this expression is used as a function but its type is %t" (Print.ty ty)
    end
