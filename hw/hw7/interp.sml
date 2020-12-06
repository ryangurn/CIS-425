(*  Concrete syntax
e :: x | n | true | false | succ | pred | iszero | if e then e else e
			 | fn x => e | e e | (e) | let x = e in e

Abstract Syntax
datatype term = AST_ID of string | AST_NUM of int | AST_BOOL of bool
	| AST_SUCC | AST_PRED | AST_ISZERO | AST_IF of (term * term * term)
	| AST_FUN of (string * term) | AST_APP of (term * term)
	| AST_LET of (string * term * term)
	| AST_ERROR of string
*)

use "parser.sml";

(*datatype term = AST_ID of string | AST_NUM of int | AST_BOOL of bool
  | AST_SUCC | AST_PRED | AST_ISZERO | AST_ADD | AST_IF of (term * term * term)
  | AST_FUN of (string * term) | AST_APP of (term * term)
  | AST_LET of (string * term * term) | AST_REC of (string * term)
  | AST_ERROR of string*)

datatype result = RES_ERROR of string | RES_NUM of int| RES_BOOL  of bool
								| RES_SUCC | RES_PRED | RES_ISZERO | RES_FUN of (string * term) 
								|  RES_FUN2 of (string * term * env2) 
							and env2 = Env2  of (string -> term * env2);

exception UnboundID
datatype env = Env of (string -> term)
fun emptyenvFun  (x : string) : term = raise UnboundID;
val emptyenv = Env emptyenvFun;
fun update (Env e) (x : string) (ty : term) = fn y => if x = y then ty else e y;


(*datatype env2 = Env2  of (string -> term * env2);*)
fun emptyenvFun2  (x : string) : (term * env2) = raise UnboundID;
val emptyenv2 = Env2 emptyenvFun2;
fun update2 (Env2 e1) (Env2 e2) (x : string) (ty : term) = fn y => if x = y then (ty, Env2 (e2)) else (e1 y);


exception Not_implemented_yet
exception Error of string

fun lookup (Env env) s = env s
fun lookup2 (Env2 env) s = env s

fun interp_lazy (env, AST_ID i)          = interp_lazy(env, lookup env i)
	| interp_lazy (env, AST_NUM n)         = RES_NUM n
	| interp_lazy (env, AST_BOOL b)        = RES_BOOL b
	| interp_lazy (env, AST_FUN (i,e))     = RES_FUN (i,e)
	| interp_lazy (env, AST_APP (AST_APP (AST_ADD, e1), e2)) = 
											 (case interp_lazy (env, e1) of
													RES_NUM n1 => (case interp_lazy (env, e2) of
																					RES_NUM n2 => RES_NUM (n1+n2)
																				|  _         => raise (Error  "AST_ADD not a number"))
												|  _         => raise (Error "AST_ADD not a number")
											 )
	| interp_lazy (env, AST_APP (e1,e2))   = (case interp_lazy (env, e1) of
				 RES_FUN (v, body) => interp_lazy(Env (update env v e2), body)
			 | RES_SUCC => (case interp_lazy(env, e2) of
													RES_NUM n1 => RES_NUM (n1+1)
												| _ => RES_ERROR "RES_SUCC argument not a number")
			 | RES_PRED =>  (case interp_lazy(env, e2) of
														RES_NUM n1 => RES_NUM(if (n1 = 0) then 0 else (n1-1))
													| _          => RES_ERROR "RES_PRED argument not a number")
			 | RES_ISZERO => (case interp_lazy(env, e2) of
														RES_NUM n1 => RES_BOOL (n1 = 0)
													| _          => RES_ERROR "RES_ISZERO argument not a number")
			 | _ => raise Error "apply non-function")
	| interp_lazy (env, AST_SUCC)          = RES_SUCC
	| interp_lazy (env, AST_PRED)          = RES_PRED
	| interp_lazy (env, AST_ISZERO)        = RES_ISZERO
	| interp_lazy (env, AST_IF (e1,e2,e3)) =  (case interp_lazy (env,e1) of
																		 RES_BOOL true  => interp_lazy (env,e2)
																	 | RES_BOOL false => interp_lazy (env,e3)
																	 | _              => raise Error  "case on non-bool")

	| interp_lazy (env, AST_LET (x,e1,e2)) = interp_lazy(Env(update env x e1), e2)
	| interp_lazy (env, AST_ERROR s)       = raise Error s
	| interp_lazy (_, _) = raise Error "something profound happened"


fun interp_lazy_static (env, AST_ID i)          = let val (e, env') = lookup2 env i in interp_lazy_static(env', e) end
	| interp_lazy_static (env, AST_NUM n)         = RES_NUM n
	| interp_lazy_static (env, AST_BOOL b)        = RES_BOOL b
	| interp_lazy_static (env, AST_FUN (i,e))     = RES_FUN2 (i,e, env)
	| interp_lazy_static (env, AST_APP (AST_APP (AST_ADD, e1), e2)) = 
											 (case interp_lazy_static (env, e1) of
													RES_NUM n1 => (case interp_lazy_static (env, e2) of
																					RES_NUM n2 => RES_NUM (n1+n2)
																				|  _         => raise (Error  "AST_ADD not a number"))
												|  _         => raise (Error "AST_ADD not a number")
											 )
	| interp_lazy_static (env, AST_APP (e1,e2))   = (case interp_lazy_static (env, e1) of
				 RES_FUN2 (v, body, env') => interp_lazy_static(Env2 (update2 env' env v e2), body)
			 | RES_SUCC => (case interp_lazy_static(env, e2) of
													RES_NUM n1 => RES_NUM (n1+1)
												| _ => RES_ERROR "RES_SUCC argument not a number")
			 | RES_PRED =>  (case interp_lazy_static(env, e2) of
														RES_NUM n1 => RES_NUM(if (n1 = 0) then 0 else (n1-1))
													| _          => RES_ERROR "RES_PRED argument not a number")
			 | RES_ISZERO => (case interp_lazy_static(env, e2) of
														RES_NUM n1 => RES_BOOL (n1 = 0)
													| _          => RES_ERROR "RES_ISZERO argument not a number")
			 | _ => raise Error "apply non-function")
	| interp_lazy_static (env, AST_SUCC)          = RES_SUCC
	| interp_lazy_static (env, AST_PRED)          = RES_PRED
	| interp_lazy_static (env, AST_ISZERO)        = RES_ISZERO
	| interp_lazy_static (env, AST_IF (e1,e2,e3)) =  (case interp_lazy_static (env,e1) of
																		 RES_BOOL true  => interp_lazy_static (env,e2)
																	 | RES_BOOL false => interp_lazy_static (env,e3)
																	 | _              => raise Error  "case on non-bool")

	| interp_lazy_static (env, AST_LET (x,e1,e2)) = raise Not_implemented_yet
	| interp_lazy_static (env, AST_ERROR s)       = raise Error s
	| interp_lazy_static (_, _) = raise Error "something profound happened"

 


fun interp_s s = interp_lazy_static(emptyenv2, (parsestr s));
fun interp_d s = interp_lazy(emptyenv, (parsestr s));


