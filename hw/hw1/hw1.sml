(* #1 *)
fun is_older (prior: int * int * int, younger: int * int * int) = 
	let 
		val py = #3 prior;
		val pm = #2 prior;
		val pd = #1 prior;
		val yy = #3 younger;
		val ym = #2 younger;
		val yd = #1 younger;
	in
		py < yy
		orelse (py < yy andalso pm < ym)
		orelse (py < yy andalso pm = ym andalso pd < yd)
	end

(* #2 *)
fun number_in_month (dates : (int * int * int) list, month : int) = 
	if null dates					(*recursive base case*)
	then 0							(*return 0*)
	else if #2 (hd dates) = month				(*if the head element month is equal to the given month*)
	then 1 + number_in_month(tl dates, month)	(*then we add one to a recursive call*)
	else number_in_month(tl dates, month)		(*if the month does not match, just continue the rest of the recursive call*)

(* #3 *)
fun number_in_months (dates: (int * int * int) list, months: int list) =
	if null months
	then 0
	else number_in_month(dates, hd months) + number_in_months(dates, tl months)

(* #4 *)
fun dates_in_month (dates: (int * int * int) list, month: int) =
	if null dates
	then []
	else if #2 (hd dates) = month
		then (hd dates) :: dates_in_month(tl dates, month)
		else dates_in_month(tl dates, month)

(* #5 *)
fun dates_in_months (dates: (int * int * int) list, months: int list) =
	if null months
	then []
	else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(* #6 *)
fun get_nth (xs: string list, n: int) = 
	if n=1
	then hd xs
	else get_nth(tl xs, n-1)

(* #7 *)
fun date_to_string(date: int * int * int) = 
	let 
		val names = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
	in
		get_nth(names, #2 date) ^ "-" ^ Int.toString(#1 date) ^ "-" ^ Int.toString(#3 date)
	end

(* #8 *)
fun number_before_reaching_sum (sum: int, xs: int list) =
	if sum <= hd xs
	then 0
	else 1 + number_before_reaching_sum (sum - hd xs, tl xs)

(* #9 *)
fun what_month (day_of_year: int) = 
	let 
		val month_lengths = [31, 28,31,30,31,30,31,31,30,31,30,31];
	in 
		1 + number_before_reaching_sum (day_of_year, month_lengths)
	end

(* #10 *)
fun month_range (day1: int, day2: int) = 
	if day1 > day2
	then []
	else what_month day1 :: month_range(day1 + 1, day2);

(* #11 *)
fun oldest (dates: (int * int * int) list) =
	if null dates
	then NONE
	else 
		let 
			val ans = oldest(tl dates)
		in 
			if isSome ans andalso is_older(valOf ans, hd dates)
			then ans
			else SOME(hd dates)
		end

(* #12 *)
fun cumulative_sum2 (sum: int, xs: int list) =
	if null xs
	then []
	else (sum + hd xs) :: cumulative_sum2(sum + hd xs, tl xs)

fun cumulative_sum(xs: int list) = 
	cumulative_sum2(0, xs)