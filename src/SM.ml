open GT       
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Syntax.Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)                         
let eval_inter conf prog =
	let (stack, f) = conf in
	let (st, input, output) = f in
	match prog with
    | BINOP operator -> (match stack with
                          | y::x::rest -> [Syntax.Expr.eval_operator operator x y] @ rest, f
                        )
    | CONST x -> [x] @ stack, f
    | READ -> (match input with
    		              | x::rest -> [x] @ stack, (st, rest, output)
              )
    | WRITE -> (match stack with
    		              | x::rest -> rest, (st, input, output @ [x])
               )
	| LD var -> [st var] @ stack, f
	| ST var -> (match stack with
		              | x::rest -> rest, (Syntax.Expr.update var x st, input, output)
                )

let eval conf program = List.fold_left eval_inter conf program

(* Top-level evaluation

     val run : int list -> prg -> int list

   Takes an input stream, a program, and returns an output stream this program calculates
*)
let run i p = let (_, (_, _, o)) = eval ([], (Syntax.Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Syntax.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)

let rec eval_expr expr = match expr with
    | Syntax.Expr.Const v -> [CONST v]
    | Syntax.Expr.Var x -> [LD x]
	| Syntax.Expr.Binop (operator, left, right) -> (eval_expr left) @ (eval_expr right) @ [BINOP operator]

let rec compile program = match program with
    | Syntax.Stmt.Read cur -> [READ; ST cur]
	| Syntax.Stmt.Write expr -> (eval_expr expr) @ [WRITE]
	| Syntax.Stmt.Assign (cur, expr) -> (eval_expr expr) @ [ST cur]
	| Syntax.Stmt.Seq (f, s) -> (compile f) @ (compile s)