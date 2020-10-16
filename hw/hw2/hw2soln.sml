datatype json =
         Num of real (* real is what SML calls floating point numbers *)
       | String of string
       | False
       | True
       | Null
       | Array of json list
       | Object of (string * json) list


fun dedup xs = ListMergeSort.uniqueSort String.compare xs
val strcmp = String.compare
val int_to_real = Real.fromInt
val real_abs = Real.abs
val real_to_string = Real.toString
val real_is_negative = Real.signBit

(* #1 *)
fun make_silly_json i = 
	let
		fun loop i =
			then []
			else (Object [("n", (int_to_real i)), ("b", True)]) :: loop (i-1)
	in
		Array (loop i)
	end

(*#2*)
(*okay polyEqual warning*)
fun assoc (k, xs) =
	case xs of 
		[] 					=> NONE
		| (k1, v1) :: xs 	=> if k = k1 then SOME v1 else assoc (k, xs)

(*#3*)
fun dot (j, f) = 
	case j of
		Object fs 			=> assoc (f, fs)
		| _					=> NONE

(*#4*)
fun one_fields j =
	let fun loop (fs, acc) =
		case fs of
			[]						=> acc
			| (k, _) :: fs 			=> loop (fs, k::acc)
	in
		case j of
			Object fs => loop (fs, [])
			| _ => []
	end

(*#5*)
fun no_repeats xs = length xs = length (dedup xs)

(*#6*)
fun recursive_no_field_repeats j =
	let
		fun traverse_array xs = 
			case xs of
				[] => True
				| x::xs => recursive_no_field_repeats x andalso traverse_array xs
		fun traverse_object xs =
			case xs of
				[] => True
				| (k, v) :: xs => recursive_no_field_repeats v andalso traverse_object xs
	in
		case j of 
			Object xs => no_repeats (one_fields j) andalso traverse_object xs
			| Array xs => traverse_array xs
			| _ => True
	end

(*#7*)
fun count_occurrences (xs, e) =
	let
		fun loop (s, n, acc, xs) =
			case xs of
				[] => (s, n) :: acc
				| x::xs => case strcmp (x, s) of
					EQUAL => loop (s, n + 1, acc, xs)
					| LESS => raise e
					| GREATER => loop (x, 1, (s, n)::acc, xs)
	in
		case xs of 
			[] => []
			| x::xs => loop (x, 1, [], xs)
	end

(*#8*)
fun string_values_for_field (field, js) = 
	case js of
		[] => []
		| x::js => case dot of
			SOME (String s) => s::string_values_for_field (field, js)
			| _ => string_values_for_field (field, js)

(*#16*)
fun concat_with (sep: string, ss: string list) : string = 
	case ss of
		[] => ""
		| [s] => s
		| s::ss => s ^ sep ^ concat_with (sep, ss)

(*#17*)
fun quote_string (s: string) : string = 
	"\"" ^ s ^ "\""

(*#18*)
fun real_to_string_for_json n = 
	(if real_is_negative n then "-" else "") ^ real_to_string (real_abs n)

(*#19*)
fun json_to_string (j: json) : string = 
	case j of
		Num n => real_to_string_for_json n
		| String s => quote_string s
		| False => "false"
		| True => "true"
		| Null => "null"
		| Array l =>
			let
				fun loop l =
					case l of
						[] => []
						| x::l => json_to_string x :: loop l
			in
				"["^ concat_with(", ", loop l)^"]"
			end
		| Object l => 
			let
				fun loop l = 
					case l of
						[] => []
						| (k, v) :: l => quote_string k ^ " : " ^ json_to_string v :: loop l
			in
				"{" ^ concat_with (", ", loop l ) ^ "}"
			end
