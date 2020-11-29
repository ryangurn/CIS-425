(*  Concrete syntax

e :: x | n | true | false | succ | pred | iszero | if e then e else e
			 | fn x => e | e e | (e) | let x = e in e

*)
(* The following definition in in the parser module 
datatype term = AST_ID of string | AST_NUM of int  | AST_BOOL of bool| 
								AST_SUCC | AST_PRED | AST_ISZERO | AST_ADD |
								AST_IF of term * term * term | 
								AST_FUN of string * term | AST_APP of term * term| 
								AST_LET of (string * term * term)| AST_ERROR of string |
								AST_REC of (string * term)

*)

use "parser.sml";

datatype result = RES_ERROR of string | RES_NUM of int| RES_BOOL of bool |
									RES_SUCC | RES_PRED | RES_ISZERO |
									RES_FUN of (string * term) | RES_CLOSURE of (string * term * env)
and env    = Env of (string -> result)

exception  Error of string
exception  UnboundID of string ;
exception Not_implemented

fun emptyenvFun  (x : string) : result = raise UnboundID x;

val emptyenv = Env emptyenvFun

fun look_up (Env e) x = e x 

(* e [x=v]  update e x v *)

(*  update : env -> string -> result -> string -> result  *)
fun update env (x : string) (value : result) =
									fn y => if x = y then value else look_up env y


(*Solution for dynamic interp*)
fun interp (env, e) = 
	case (env, e) of
		(env, AST_ID x)         => look_up env x
		| (env, AST_NUM n)      => if (n <= 0) then RES_NUM 0 else RES_NUM n
		| (env, AST_BOOL b)     => RES_BOOL b
		| (env, AST_SUCC)       => RES_SUCC
		| (env, AST_PRED)       => RES_PRED
		| (env, AST_FUN(p, b))  => RES_FUN(p, b)
		| (env, AST_APP(AST_ADD, x)) => raise Error "missing a parameter to plus"
		| (env, AST_APP(AST_APP(AST_ADD, x), y)) => (case (interp(env, x), interp(env, y)) of
																									(RES_NUM x, RES_NUM y) => RES_NUM (x+y)
																									| _ => raise Error "trying to add a non-number"
																								)
		| (env, AST_IF(p, t, f)) => (case interp(env, p) of
																	RES_BOOL true => interp(env, t)
																	| RES_BOOL false => interp(env, f)
																	| _ => raise Error "non-predicate in if clause")
		| (env, AST_APP(f, p))  => (case (interp(env, f), interp(env, p)) of
																(RES_SUCC, RES_NUM n)             => RES_NUM (n + 1)
																| (RES_PRED, RES_NUM n)           => if (n <= 0) then RES_NUM 0 else RES_NUM (n - 1)
																| (RES_FUN(formal, b), actual)  => interp(Env (update env formal actual), b)
																| _                               => raise Error "incorrect term in function application")
		| (env, AST_LET(x, e1, e)) => interp(Env (update env x (interp(env, e1))), e)
		| _                     => raise Error "invalid"

(*solution for static*)
fun interp_static (env, e) = 
	case (env, e) of
		(env, AST_ID x)         => look_up env x
		| (env, AST_NUM n)      => if (n <= 0) then RES_NUM 0 else RES_NUM n
		| (env, AST_BOOL b)     => RES_BOOL b
		| (env, AST_SUCC)       => RES_SUCC
		| (env, AST_PRED)       => RES_PRED
		| (env, AST_FUN(p, b))  => RES_CLOSURE(p, b, env)
		| (env, AST_APP(AST_ADD, x)) => raise Error "missing a parameter to plus"
		| (env, AST_APP(AST_APP(AST_ADD, x), y)) => (case (interp_static(env, x), interp_static(env, y)) of
																									(RES_NUM x, RES_NUM y) => RES_NUM (x+y)
																									| _ => raise Error "trying to add a non-number"
																								)
		| (env, AST_IF(p, t, f)) => (case interp_static(env, p) of
																	RES_BOOL true => interp_static(env, t)
																	| RES_BOOL false => interp_static(env, f)
																	| _ => raise Error "non-predicate in if clause")
		| (env, AST_APP(f, p))  => (case (interp_static(env, f), interp_static(env, p)) of
																(RES_SUCC, RES_NUM n)             => RES_NUM (n + 1)
																| (RES_PRED, RES_NUM n)           => if (n <= 0) then RES_NUM 0 else RES_NUM (n - 1)
																| (RES_CLOSURE(x, e, env1), e2)  => interp_static(Env(update env1 x e2), e)
																| _                               => raise Error "incorrect term in function application")
		| (env, AST_LET(x, e1, e)) => interp_static(Env (update env x (interp_static(env, e1))), e)
		| _                     => raise Error "invalid"
 

fun interp  (env, AST_SUCC)          = RES_SUCC
|   interp  (env, AST_PRED)          = RES_PRED
|   interp  (env, AST_ISZERO)        = RES_ISZERO
|   interp  (env, AST_NUM n)         = RES_NUM n
|   interp  (env, AST_BOOL b)        = RES_BOOL b
|   interp  (env, AST_ID x  )        = look_up env x 
|   interp  (env, AST_IF (e1,e2,e3)) = (case interp (env,e1) of
														RES_BOOL true  => interp  (env,e2)
													| RES_BOOL false => interp  (env,e3)
													| _              => raise 
																						 (Error "if condition non-bool!") )

|   interp  (env, AST_FUN(x,body))   = RES_FUN (x,body)
|   interp  (env, AST_LET (x,e1,e2)) = let val v1 = interp (env, e1)
																				in interp (Env (update env x v1), e2)
																			 end

	 
|  interp  (env, AST_APP(AST_APP(AST_ADD,e1),e2)) =  (* + e1 e2 *)
						 (case interp (env, e1) of
								RES_NUM n1 => (case interp (env, e2) of
																	 RES_NUM n2 => RES_NUM (n1+n2)
																	| _         => raise (Error "not a number"))
							| _          => raise (Error "not a number") 
											 )
| interp (env, AST_APP(AST_ADD,e1)) = raise (Error "not enough arguments for +") (* + e1 *)
| interp   (env, AST_APP (e1,e2))   = 
			 (case interp  (env, e1) of
				 RES_FUN (formal, body) => let val actual  = interp (env, e2) 
																		 val new_env = Env (update env formal actual)
																	in interp (new_env, body)
																	end  
			 | RES_SUCC =>
				 (case interp (env,e2) of
							RES_NUM n => RES_NUM (n+1)
						| _ => raise (Error "succ non-number"))
			 | RES_PRED =>
				 (case interp (env,e2) of
							RES_NUM n => RES_NUM (n+1)
						| _ => raise (Error "pred non-number"))
			 | RES_ISZERO =>
				 (case interp (env,e2) of
							RES_NUM n => RES_BOOL (n = 0)
						| _ => raise (Error "iszero non-number"))
			 | _ => raise (Error "apply non-function") )
 |  interp (env, AST_ERROR s)       = raise (Error s)
 |  interp (env, _)                = raise Not_implemented;


val test_string = "let x = 2 in let f = fn z => x  in let  x = 100 in (f x) end end end";


val test = AST_LET ("x",  AST_NUM 2,
						AST_LET ("f", AST_FUN ("z", AST_ID "x"),
						 AST_LET("x", AST_NUM 100, 
							AST_APP (AST_ID "f", AST_ID "x"))));

val resul_test = interp (emptyenv, test);

val x = parsestr test_string;

fun interp_1 s = interp (emptyenv, parsestr s)
									handle (Error z) =>  RES_ERROR z
								 |       (UnboundID x) => RES_ERROR("Variable Unbound " ^ x );

interp_1 test_string;

val test1 = "let x = 1 in \
							\let f = fn y => let  w = + y 1 \
								\                in fn z => + (+ (+ x y) z) w \
									\             end \
									\in let x = 8 \
										\ in let g = f 6 \
											\   in let y = 5 \
												\    in g 3 \
												\ end end end end end";
												
interp_1 test1;

val test2 = "let f  = fn  g => let x = 3 in g 2 end \
							 \in  let x = 4  \
								\    in let h = fn y => + x y   \
								 \      in f h \
									 \    end end end";


interp_1 test2;
