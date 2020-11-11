use "type.sml";

Control.Print.printDepth:= 100;

(* A term datatype with typed function arguments to allow type checking *)
datatype term
  = AST_ID of string
  | AST_NUM of int
  | AST_BOOL of bool
  | AST_FUN of (string * typ * term)
  | AST_SUCC
  | AST_PRED
  | AST_ISZERO
  | AST_IF of (term * term * term)
  | AST_APP of (term * term)
  | AST_LET of (string * term * term)

exception Unimplemented
exception TypeError


fun lookup env x = case env of 
  Env env => env x

(* typeOf : env -> term -> typ *)
fun typeOf env (AST_ID s)          =  lookup env s
  | typeOf env (AST_NUM n)         = INT
  | typeOf env (AST_BOOL b)        = BOOL
  | typeOf env (AST_FUN (x,t1,e))   = ARROW(t1, typeOf (Env(update env x t1)) e)
  | typeOf env AST_SUCC            = ARROW(INT, INT)
  | typeOf env AST_PRED            = ARROW(INT, INT) 
  | typeOf env AST_ISZERO          = ARROW (INT,BOOL)
  | typeOf env (AST_IF (e1,e2,e3)) =
    let val t1 = typeOf env e1
        val t2 = typeOf env e2
        val t3 = typeOf env e3
    in
        case (t1 = BOOL) andalso (t2 = t3) of
            true => t3
          | false => raise TypeError
    end
  | typeOf env (AST_APP (e1,e2))   = (case (typeOf env e1, typeOf env e2) of
    (ARROW(t1, t2), t1')   => if t1 = t1' then t2 else raise TypeError
    | _             => raise TypeError)
  | typeOf env (AST_LET (x,e1,e2)) = let
    val t1 = typeOf env e1
    val env' = Env (update env x t1)
  in
    typeOf env' e2
  end

(*
Some sample functions translated into abstract syntax for you to test
your typechecker on:
*)

(* fn (f : a -> a) => fn (x : a) => f (f x) *)
val test1 = AST_FUN("f", ARROW(VAR "a", VAR "a"),
                AST_FUN("x", VAR "a",
                    AST_APP(AST_ID "f",
                        AST_APP(AST_ID "f", AST_ID "x"))));

(* fn (f : 'b -> 'c) => fn (g : 'a -> 'b) => fn (x : 'a) => f (g x) *)
val test2 = AST_FUN("f", ARROW(VAR "b", VAR "c"),
                AST_FUN("g", ARROW(VAR "a", VAR "b"),
                    AST_FUN("x", VAR "a",
                        AST_APP(AST_ID "f",
                            AST_APP(AST_ID "g", AST_ID "x")))));

(* (* fn (b : bool) => if b then 1 else 0 *) *)
val test3 = AST_FUN("b", BOOL,
                AST_IF(AST_ID "b", AST_NUM 1, AST_NUM 0));

val test3x = fn (b: bool) => if b then 1 else 0;

(* feel free to write your own test expressions! *)
(*ARROW (ARROW (VAR "a",VAR "a"),ARROW (VAR "a",VAR "a"))*)
typeOf emptyenv test1;
(*ARROW(ARROW (VAR "b",VAR "c"), ARROW (ARROW (VAR "a",VAR "b"),ARROW (VAR "a",VAR "c")))*)
typeOf emptyenv test2;
(*ARROW (BOOL,INT)*)
typeOf emptyenv test3;
