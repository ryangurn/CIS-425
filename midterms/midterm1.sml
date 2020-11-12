fun is_older (date1: int * int * int, date2: int * int * int) = 
		let val y1 = #3 date1
			val m1 = #2 date1
			val d1 = #1 date1
			val y2 = #3 date2
			val m2 = #2 date2
			val d2 = #1 date2
		in
			y1 < y2
			orelse (y1=y2 andalso m1<m2)
			orelse (y1=y2 andalso m1=m2 andalso d1 < d2)
		end;
(* #1 *)
(*is_older((2,3,4), (3,4,5));*)
is_older ((2,3,4), (3,4,5));
(*is_older(3, (3,4,5));*)
is_older ((3,3,3), (3,4,5));
(*is_older(3,4,5), (3,4,5);*)
is_older ((3,4,5), (3,4,5));

(* #2 *)
(* (A) *)
fun dates_in_monthA (dates, month) = 
	case dates of
		[] => []
		| ((d, m, y) :: dates') => if m = month
			then 
				(d, m, y) :: dates_in_monthA(dates', m)
			else
				dates_in_monthA(dates', month)

(* (B) *)
fun dates_in_monthB (dates: (int * int * int) list, month) = 
	if null dates
	then []
	else if #2 (hd dates) = month
		then (hd dates)::dates_in_monthB(tl dates, month)
		else dates_in_monthB(tl dates, month);

(* you cannot remove the type in this otherwise you will get a flex error *)
(* this is because you cannot figure out the types for #1 & #3 or know the length *)
(* of the list itself *)

(* (C) *)
(* the function written is polymorphic using an equality type for the month *)


(* #3 *)
exception Error

fun get_nth (xs: string list, n: int) = 
	case xs of 
		[]	=> raise Error
		| x::xs' => if n = 1 then x else get_nth(xs', n - 1)

(* #4 *)
(* (A) why is the function not tail recursive? *)
(* no, because there is no accumulator and is using the plus operator which happens at the end *)

(* (B) What are the advantages*)
(* most smart compilers can go from O(n) to O(1) just like a for loop*)

(* (C) *)
fun number_before_reaching_sumC (sum: int, xs: int list, acc) = 
	case xs of
		[] => acc
		| x::xs' => if sum <= x
					then acc
					else number_before_reaching_sumC (sum - x, xs', (acc + 1));

number_before_reaching_sumC(3, [1, 1, 1, 3], 0);

(* (D) *)
fun number_before_reaching_sumD (sum: int) (xs: int list) acc = 
	case xs of 
		[] => acc
		|x::xs' => if sum <= x
					then acc
					else number_before_reaching_sumD (sum - x) xs' (acc + 1);

number_before_reaching_sumD 3 [1,1,1,3] 0;

(* no acc *)
fun number_before_reaching_sum_no_acc (sum: int) (xs: int list) = 
	let
		fun x1 (sum: int) (xs: int list) acc = 
			case xs of 
				[] => acc
				| x::xs' => if sum <= x 
							then acc 
							else x1 (sum - x) (xs') (acc + 1)
	in
		x1 sum xs 0
	end;

number_before_reaching_sum_no_acc 3 [1,1,1,3];

(* #5 *)
datatype tree = I of int | S of string | N of tree * tree

(* (A) *)
val threeIntTree = N(N(I 2, I 3), I 1)

(* (B) *)
fun sum t =
	case t of
		I i => i
		| S _ => 0
		| N(l, r) => (sum l) + (sum r);

sum threeIntTree;

(* #6 *)
(* (A) *)
(* the types do not match for the case x::[], thus not all branches match and so we need to make it a list *)

(* (B) *)
fun f3 xs = 
	case xs of
		[] => 0::[]
		| x::[] => [x+1] (* this was previously just x+1 without brackets*)
		| x::xs => (x+1)::(f3 xs);

(* #7 *)
val is_even = List.filter (fn x => (x mod 2) = 0);

is_even [1,2,3,4,5,6,7,8,9];

(* #8 *)
let val x = 2 (*[(x,2)]*)
	in let fun f z = x + z (*[(x,2)]*)
		in let val x = 100 (*[(x,100)]*)
			in (f x) + x (*[(x,100)]*)
			end
		end
	end;

(* static (f x) + x -> (f 100) + 100 -> (100 + 2) + 100 -> 202 *)
(* dynamic (f x) + x -> (f 100) + 100 -> (100 + 100) + 100 -> 300 *)

val x = 1 (*[(x,1)]*)
fun f y = (*[(x,1)]*)
	let val w = y + 1 in (*[(x,1), (w, y+1)]*)
		fn z => x + y + z + w (*[(x,1), (w, y+1)]*)
	end

val x = 8 (*[(x,8)]*)
val y = 5 (*[(x,8), (y, 5)]*)
val g = f 6 (*[(x,1), (y, 6), (w, 7), (g, fn z => x + y + z + w)]*)
val z = g 3 (*[(x,1), (y, 6), (z, 3) (w, 7), (g, fn z => x + y + z + w)]*)
			(*1 + 6 + 7 + 3 = 17*)

val x = 1 (*[(x,1)]*)
fun f y = (*[(x,1)]*)
	let val w = y + 1 in (*[(x,1), (w, y + 1)]*)
		fn z => x + y + z + w (*[(x,1), (w, y + 1)]*)
	end

val x = 8 (*[(x,8)]*)
val y = 5 (*[(x,8), (y, 5)]*)
val g = f 6 (*[(x,1), (y, 6), (w, 7), (g, fn z => x + y + z + w)]*)
val z = g 3 (*[(x,1), (y, 6), , (g, fn z => x + y + z + w)]*)
			(*unbound var error*)

			