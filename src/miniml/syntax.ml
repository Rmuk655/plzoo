(* Abstract syntax of the miniml language *)

(* Variable names *)
type name = string

(* Types *)
type ty =
  | TInt              (* Integers *)
  | TBool             (* Booleans *)
  | TArrow of ty * ty (* Functions *)
  | TString           (* String type*)
  | TVar of ty option ref  
  (* Type variables which is used to represent the type of the variable when it's type is not known.*)

(* 
  The different exceptions that can be raised.
  - DivisionByZero: Used when there is a division by zero.
  - GenericException of int: A general error with an integer value.
*)
type exception_value =
  | DivisionByZero
  | GenericException of int

(* Typed abstract syntax tree (AST) of Expressions in Miniml *)
type expr = expr' Zoo.located
and expr' =
  | Var of name          		(* Variable *)
  | Int of int           		(* Non-negative integer constant *)
  | Bool of bool         		(* Boolean constant *)
  | Times of expr * expr 		(* Product [e1 * e2] *)
  | Divide of expr * expr   (* Defining the syntax for Division [e1 / e2] of two expressions e1, e2.*)
  | Plus of expr * expr  		(* Sum [e1 + e2] *)
  | Minus of expr * expr 		(* Difference [e1 - e2] *)
  | Equal of expr * expr 		(* Integer comparison [e1 = e2] *)
  | Less of expr * expr  		(* Integer comparison [e1 < e2] *)
  | If of expr * expr * expr 		(* Conditional [if e1 then e2 else e3] *)
  | Fun of name * name * ty * ty * expr (* Function [fun f(x:s):t is e] *)
  | Apply of expr * expr 		(* Application [e1 e2] *)
  
  | Raise of exception_value  (* Raise DivisionByZero or GenericException(int n) *)
  | TryWith of expr * exception_value * expr  (* Try e1 with DivisionByZero -> e2 or Try e1 with GenericException(n) -> e2 *)
  | String of string  (* String literal *)
  (* Examples: 
      try 10 / 0 with DivisionByZero -> 420
      try 10 / 0 with GenericException(5) -> 5
      raise DivisionByZero
      raise GenericException(42)
    *)
(* Toplevel commands *)
type command = 
  | Expr of expr       (* Expression *)
  | Def of name * expr (* Value definition [let x = e] *)
