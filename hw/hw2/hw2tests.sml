(* CSE 341, Homework 2 Tests *)

use "hw2.sml";

(* You will surely want to add more! *)

(* warning: because real is not an eqtype, json is not an eqtype, so you cannot 
   use = on anything including something of type json.
   See test1, test3, and test9 for examples of how to work around this. *)

val epsilon = 0.0001
fun check_real (r1,r2) = Real.abs (r1 - r2) < epsilon

val test1 =
    case make_silly_json 2 of
        Array [Object [("n",Num x),
                       ("b",True)],
               Object [("n",Num y),
                       ("b",True)]]
        => check_real (x,2.0) andalso check_real(y,1.0)
      | _ => false

val test2 = assoc ("foo", [("bar",17),("foo",19)]) = SOME 19

val test3 = case dot (json_obj, "ok") of SOME True => true |  _ => false

val test4 = one_fields json_obj = rev ["foo","bar","ok"]

val test5 = not (no_repeats ["foo","bar","foo"])

val nest = Array [Object [],
                  Object[("a",True),
                         ("b",Object[("foo",True),
                                     ("foo",True)]),
                         ("c",True)],
                  Object []]

val test6 = not (recursive_no_field_repeats nest)

 (* any order is okay, so it's okay to fail this test due to order *)
val test7a = count_occurrences (["a", "a", "b"], Fail "") = [("b",1),("a",2)]

val test7b = count_occurrences (["b", "a", "b"], Fail "") = []
             handle (Fail "") => true

val test8 = string_values_for_field ("x", [Object [("a", True),("x", String "foo")],
                                           Object [("x", String "bar"), ("b", True)]])
            = ["foo","bar"]

val test9 = 
    case filter_field_value ("x", "foo",
                             [Object [("x", String "foo"), ("y", String "bar")],
                              Object [("x", String "foo"), ("y", String "baz")],
                              Object [("x", String "a")],
                              Object []]) of
        [Object [("x",String "foo"),("y",String "bar")],
         Object [("x",String "foo"),("y",String "baz")]] => true
      | _ => false

(*****)                 

val test16 = concat_with("a",["b","n","na"]) = "banana"

val test17 = quote_string "foo" = "\"foo\""

val test18 = real_to_string_for_json ~4.305 = "-4.305"

val test19 = json_to_string json_obj = 
             "{\"foo\" : 3.14159, \"bar\" : [1.0, \"world\", null], \"ok\" : true}"

(* End of tests for required problems. A few commented-out tests for
   challenge problems follow.  The tests below are in a different style 
   where we use pattern-matching in val-bindings for the expected output. *)

(*
(* Tests for consume_string_literal *)
val ("foo",[#" ",#":",#" ",#"t",#"r",#"u",#"e"]) =
  consume_string_literal (String.explode "\"foo\" : true")

(* Tests for consume_keyword *)
val (FalseTok, [#" ",#"f",#"o",#"o"]) =
  consume_keyword (String.explode "false foo")

(* Tests consume_number *)
val ("1",[]) = consume_num (String.explode "1")
val ("~1.23e17",[]) = consume_num (String.explode "~1.23e17")

(* Tests for tokenize_char_list. You'll want more. *)
val [LBrace, StringLit "foo", Colon, NumLit "3.14", Comma,
     StringLit "bar", Colon, LBracket, TrueTok, Comma,
     FalseTok, RBracket, RBrace] =
  tokenize_char_list (String.explode "{ \"foo\" : 3.14, \"bar\" : [true, false] }")

(* Tests for parse_string *)
val ("foo", [FalseTok]) =
  parse_string ([StringLit "foo", FalseTok])

(* Tests for expect *)
val [FalseTok] = expect (Colon, [Colon, FalseTok])

(* Tests for parse_json. You'll probably want way more. *)
val (Object [("foo", Null),("bar",Array [True,False])],[]) =
  parse_json (tokenize "{ \"foo\" : null, \"bar\" : [true, false] }")
*)

