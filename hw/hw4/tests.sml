use "interp.sml";
(*  These tests make use of addition which is expressed in
    prefix form.  Instead of writing 2 + 3 we write
    + 2 3, its abstract syntax is
        AST_APP (AST_APP (AST_ADD, AST_NUM 2), AST_NUM 3)
*)

(*  Examples from Week 3 *)
(* 
val test1 =
let val x = 1
in let fun f z = x
   in let val x = 99
      in f x            
      end
   end
end;

Static  : 1
Dynamic : 99

*)

val test1 =  "let x = 1 in \
              \ let f = fn z => x in \
              \ let x = 99 \
              \ in f x       \      
              \ end \
           \ end \
       \  end  " ;

(* *********************************************************************** *)

(*
val test2 =
let val x = 1
in let fun f z = x
   in let val x = 99
      in (f x) +  x 
      end
   end
end;

Static:   100
Dynamic:  198
*)


val test2 = "let x = 1 \
\ in let f = fn z => x \
\   in let  x = 99 \
\      in (+ (f x)  x)  \
\      end \
\   end \
\ end ";



(* *********************************************************************** *)

(*
val x = 1;
fun f y =
    let
        val z = y+1
        val t = fn w => x + y + w + z
      in t
    end;

val x = 3;
val g = f 4; 
val z1 = g 6;   

Static:   16
Dynamic:  uncaught exception UnboundID
*)


val test3 =  "let x = 1 \
\  in let f = fn y => let  z = + y 1 \
\                     in   fn w => let t1 = + x  y \
\                                    in let t2 = + t1  w \
\                                        in + t2  z \
\                                        end \
\                                     end \
\                     end \
\      in let x = 3 \
\          in let g = f 4 \
\              in g 6 \
\             end \
\          end \
\       end \
\   end";

(* *********************************************************************** *)
(*
fun f g =
    let
        val x = 3
    in g 2
    end;
val x = 4;
fun h y = x + y ; 
val z2 = f h; 

Static:    6
Dynamic:   5
*)

val test4 = "let f = fn g => let x = 3 in g 2 end \
\             in let x = 4 \
\                in let h = fn y=> + x y \
\                    in f h \
\                   end \
\                end \
\              end";



(* *********************************************************************** *)


(* Are the following two functions the same ? 

fun f1 y =
    let
        val x = y + 1
        val t = fn z => x + y + z
      in t
    end;

fun f2 y =
    let
        val q = y + 1
        val t = fn z => q + y + z
      in t
    end;
val y = 0;
val x = 17 ;
val q = 0;
val r1 = f1 2;
val a1 = r1 4;  
val r2 = f2 2;
val a2 = r2 4;  

Static:  a1 is 9
Dynamic: a1 is 21

Static:  a2 is 9
Dynamic: a2 is 4
*)

val test5 = "let f1 = fn y=> let x = + y 1 in fn z => let t1= + x y \
\                                                      in + t1  z \
\                                                      end \
\                            end \
\             in let y = 0  \
\                 in let x = 17 \
\                     in let q = 0 \
\                         in let r1 = f1 2  \
\                            in  r1 4 \
\                            end \
\                         end \
\                    end \
\                 end \
\             end";



val test6 = "let f2 = fn y=> let q = + y 1 in fn z => let t1= + q y \
\                                                      in + t1  z \
\                                                      end \
\                            end \
\             in let y = 0  \
\                in let x = 17 \
\                   in let q = 0 \
\                      in let r1 = f2 2  \
\                             in  r1 4 \
\                             end \
\                           end \
\                        end \
\                end \
\             end";


(* f3 and f4 are always the same, no matter what argument is passed in 

fun f3 g =
    let
        val x = 3 
    in
        g 2
    end;

fun f4 g = g 2;


val x = 17;
val a3 = f3 (fn y => x + y) ; (* you notice a difference *)
val a4 = f4 (fn y => x + y) ;(* there is no difference *)

Static:   a3 is 19
Dynamic:  a3 is 5

Static:   a4 is 19
Dynamic:  a4 is 19
*)

val test7 = "let f3 = fn g => let x = 3 in g 2 end \
\             in let x = 17   \ 
\                in f3 (fn y => + x  y)  \
\                end \
\            end " ;

val test8 = "let f4 = fn g => g 2  \
\            in let x = 17  \
\                in f4 (fn y => + x y) \
\                end \
\            end";

