exception NoAnswer

datatype pattern = WildcardP
					| VariableP of string
					| UnitP
					| ConstantP of int
					| ConstructorP of string * pattern
					| TupleP of pattern list


datatype valu = Constant of int
				| Unit 
				| Constructor of string * valu
				| Tuple of valu list


(*#1*)
val only_lowercase = List.filter (fn s => Char.isLower (String.sub(s, 0)))

val x1 = only_lowercase ["ASDadsasdasd", "lkjsadlkjasd", "asdasdasdasd", "Wjwjllkjw"]

(*#2*)
val longest_string1 = List.foldl (fn (s, soFar) => 
	if (String.size s > String.size soFar) 
	then s else soFar) ""

val x2 = longest_string1 ["123456", "12345a", "123", "1234"]

(*#3*)
val longest_string2 = List.foldl (fn (s, soFar) => 
	if (String.size s >= String.size soFar) 
	then s else soFar) ""

val x3 = longest_string2 ["123456", "12345a", "123", "1234"]

(*#4*)
fun long_string_helper f = List.foldl (fn (s, soFar) => 
	if f (String.size s, String.size soFar) 
	then s else soFar) ""

(*#5*)
val longest_string3 = long_string_helper (fn (s, soFar) => s > soFar)
val x4 = longest_string3 ["123456", "12345a", "123", "1234"]

val longest_string4 = long_string_helper (fn (s, soFar) => s >= soFar)
val x5 = longest_string4 ["123456", "12345a", "123", "1234"]

(*#5*)
val longest_lowercase = longest_string1 o only_lowercase
val x6 = longest_lowercase ["ASDadsasdasd", "lkjsadlkjasd", "asdasdasdasd", "Wjwjllkjw"]

(*#6*)
val caps_no_X_string = String.implode o List.filter (fn s => s <> #"X") o String.explode o (String.map Char.toUpper)
val x7 = caps_no_X_string "asdlkjsadlkhalkjhxkljx;klhxjjhfasdklhqwkhe"

(*#7*)
fun first_answer f ps = 
	case ps of 
		[]			=> raise NoAnswer
		| p::ps' 	=> case (f p) of 
						NONE 		=> first_answer f ps'
						| SOME v	=> v 

(*#8*)
fun all_answers f ps = 
	let
		fun loop (ps, acc) = 
			case ps of
				[] => SOME acc
				| p::ps' => case (f p) of
					NONE => NONE
					|SOME y => loop (ps', y@acc)
	in
		loop (ps, [])
	end

(*#9*)
fun g f1 f2 p =
    let
        val r = g f1 f2
    in
        case p of
            WildcardP				=> f1 ()
			| VariableP x			=> f2 x
			| ConstructorP(_,p)		=> r p
			| TupleP ps				=> List.foldl (fn (p, i) => (r p) + i) 0 ps
			| _						=> 0
	end
(*a higher order function g that given a pattern p it pattern matches on it and then *)

val count_wildcards = g (fn _ => 1) (fn _ => 0)
val count_wildcards_variable_lengths = g (fn _ => 1) (fn n => String.size n)
val count_wildcards_variable_lengths_simpler = g (fn _ => 1) String.size
fun count_a_var (s, p) = g (fn _ => 0) (fn n => if (n = s) then 1 else 0) p


(*#10*)
fun dedup xs = ListMergeSort.uniqueSort String.compare xs;
fun unique xs = (length xs) = (length (dedup xs));
fun check_pat p =
	let
		fun get_vars p = case p of
			VariableP n 				=> [n]
			| TupleP ps 				=> List.foldl (fn (p, vars) => get_vars p @ vars) [] ps
			| ConstructorP (_, p) 		=> get_vars p
			| _ 						=> []  
	in
		(unique o get_vars) p
	end

(*#11*)
fun match (valu, pat) = 
	case (valu, pat) of
		(_, WildcardP) 									=> SOME []
		| (_, VariableP s) 								=> SOME [(s, valu)]
		| (Unit, UnitP) 								=> SOME []
		| (Constant i, ConstantP i') 					=> if (i' = i) then SOME [] else NONE
		| (Tuple vs, TupleP ps) => if (length vs = length ps) then 
			all_answers match (ListPair.zip(vs, ps)) else
				NONE	
		| (Constructor (s1, v), ConstructorP(s2, p)) 	=> if (s1 = s2) then 
			match (v, p) else
			NONE
		| _ 											=> NONE

(*#11*)
(* non curried*)
fun first_match (valu, ps) = 
	SOME (first_answer (fn pat => match (valu, pat)) ps)
	handle NoAnswer => NONE

(*curried*)
fun first_match valu ps = 
	SOME (first_answer (fn pat => match (valu, pat)) ps)
	handle NoAnswer => NONE