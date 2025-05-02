(** MiniML compiler. *)

open Machine

(** [compile e] compiles program [e] into a list of machine instructions. *)
(*I have written a recursive function to compile the commands passed to the miniml language.
This function ensures that each token it receives from the parser has an implementation.
Ex: Syntax.Plus (e1, e2) where e1 and e2 are expressions. The output must be e1 + e2. 
The compiler finds the ouput by first computing the values of the expressions e1, e2
using compile e1 compile e2 then adding them using the operation IAdd, whose definition
is present in the machine.ml code.*)
(*In the below code, I have added the Syntax.Divide(Division), Syntax.Raise(DivisionByZero and Generic Exception),
Syntax.TryWith(try with), Syntax.String(String data type) implementations below.*)
let rec compile {Zoo.data=e'; _} =
  match e' with
    | Syntax.Var x -> [IVar x]
    | Syntax.Int k -> [IInt k]
    | Syntax.Bool b -> [IBool b]
    | Syntax.Times (e1, e2) -> (compile e1) @ (compile e2) @ [IMult]
    | Syntax.Plus (e1, e2) -> (compile e1) @ (compile e2) @ [IAdd]
    | Syntax.Minus (e1, e2) -> (compile e1) @ (compile e2) @ [ISub]
    | Syntax.Equal (e1, e2) -> (compile e1) @ (compile e2) @ [IEqual]
    | Syntax.Less (e1, e2) -> (compile e1) @ (compile e2) @ [ILess]
    | Syntax.If (e1, e2, e3) -> (compile e1) @ [IBranch (compile e2, compile e3)]
    | Syntax.Fun (f, x, _, _, e) -> [IClosure (f, x, compile e @ [IPopEnv])]
    | Syntax.Apply (e1, e2) -> (compile e1) @ (compile e2) @ [ICall]
    | Syntax.Divide (e1, e2) -> (compile e1) @ (compile e2) @ [IDiv]  
    (*Compiles the two subexpressions (e1 and e2), 
    then applies the integer division operation (IDiv - defined in machine.ml) in the resulting code.*)
    | Syntax.Raise (DivisionByZero) -> [IPushExn "Division by zero"; IRaise]
    (*Pushes a "Division by zero" exception onto the stack and raises it (IRaise).*)
    | Syntax.Raise (GenericException n) -> let exn_msg = "Generic exception: " ^ string_of_int n in
                                        [IPushExn exn_msg; IRaise]
    (*Pushes a "Generic exception" message onto the stack and raises it (IRaise).*)
    | Syntax.TryWith (e1, exn_pat, e2) ->
      (*Compiles the first expression (e1) to handle the try block and 
      the second expression (e2) for the exception handler. Matches the exception 
      pattern (exn_pat) and creates a corresponding exception  message string 
      (e.g., "Division by zero" or "Generic exception: <n>").
      Combines the try block and the exception handling code (ITryWith) 
      in the final compiled code.*)
  
      let code1 = compile e1 in
      let code2 = compile e2 in
      let pat_string =
        match exn_pat with
        | DivisionByZero -> "Division by zero"
        | GenericException n -> "Generic exception: " ^ string_of_int n
      in
      code1 @ [ITryWith (pat_string, code2)]    
    | Syntax.String s -> [IString s] (* Compiles the string s and sends it to machine.ml which pushes the string into 
    the stack of instructions to make it available for use.*)
        