
(**** CSE341, Homework 2, CHALLENGE PROBLEMS: parsing JSON values ****)

(* basically treat this as a continuation of your hw2 solution: *)
use "hw2.sml";

(* The main assignment used pre-parsed JSON data, but now we will
   implement a parser for JSON. The course staff's solution to this
   section was used to create the pre-parsed data used earlier. With
   your own parser, you can go off and explore other datasets in SML.

   This parser is difficult but rewarding. Getting comfortable with
   parsing and with the functional programming ideas used to implement
   it will serve you well in the long run. Even if you don't finish
   the whole parser, you will learn from trying at least a few of the
   tokenization problems. *)

(* Parsing is the problem of converting a textual representation into
   a structured representation. For example, turning
       { "foo" : 3.14, "bar" : [true, false] }
   into the corresponding json value. Parsers are also used in compilers
   to convert source code into a suitable internal representation.

   Parsing a very well-studied problem in computer science. If you
   take a compilers class, you will probably spend at least two weeks
   on different parsing algorithms. This can give the impression that
   parsing is so hard that only experts should attempt it.  But
   certain parsing techniques are actually very straightforward to
   implement, especially in a functional language such as SML. *)

(* Our goal is to turn a string into a json value. We will divide the
   problem into two steps, called tokenization and parsing.

   The purpose of tokenization is to make the parsing problem easier
   by grouping related characters of the input together.

   Tokenization will turn the string into a list of "tokens", which
   are pieces of the input that "go together". It may help to think of
   tokens as the "words" or "indivisible atoms" of the input.

   For example, tokenizing the string
       { "foo" : 3.14, "bar" : [true, false] }
   will result in something like the list
       [LBrace, StrLit "foo", Colon, NumLit "3.14", Comma,
        StringLit "bar", Colon, LBracket, TrueTok, Comma,
        FalseTok, RBracket, RBrace]
   Notice how the characters representing field names have been grouped
   together, as have those representing the number 3.14. True and False
   have been represented specially, as has all punctuation. Also, all
   whitespace has been removed.

   We will generally work with lists of characters (char lists)
   instead of strings, so that we can use our favorite tools of
   recursion and pattern matching to write the tokenizer. We will use
   the standard library functions
       String.explode : string -> char list
   and
       String.implode : char list -> string
   to convert between the two representations.

   Character literals in SML are written as string literals with a
   hash in front of the opening double quote: #"a" represents the
   character 'a'.

   Our tokens will be represented by the following datatype:
*)
datatype token =
           NumLit of string (* eg, number 3.14 represented as "3.14" *)
         | StringLit of string (* eg, "foo" *)
         | FalseTok (* false *)
         | TrueTok (* true *)
         | NullTok (* null *)
         | LBrace (* { *)
         | RBrace (* } *)
         | LBracket (* [ *)
         | RBracket (* ] *)
         | Comma (* , *)
         | Colon (* : *)

(* For debugging purposes, it will be convenient to be able to print
   tokens. Here is a provided function to convert tokens to strings. *)
fun token_to_string t =
  case t of
     NumLit    s =>  s
   | StringLit s => quote_string s (* you wrote quote_string in hw2.sml *)
   | FalseTok    => "false"
   | TrueTok     => "true"
   | NullTok     => "null"
   | LBrace      => "{"
   | RBrace      => "}"
   | LBracket    => "["
   | RBracket    => "]"
   | Comma       => ","
   | Colon       => ":"

(* Here's another provided function to report lexical errors conveniently.
   ("Lexical analysis" is another name for "tokenization".) *)
fun lexical_error msg = raise Fail ("Lexical error: " ^ msg)

(* The general idea of the tokenizer will be to examine the beginning
   of the character list to find the next token, "consume" it, and
   then continue to process the rest of the list. Here, "consuming"
   just means to process the first few characters of the list, and
   return the rest of them for later processing.

   In other words, a function to consume a foo will have type
       char list -> foo * char list
   that is, it takes the char list, and converts the first bit of it
   into a foo, and returns the remainder of the list.

   We will write three consumers, one for string literals, one for
   keywords (false, true, and null), and one for numeric literals.
 *)

(* Challenge Problem C1: Write a consumer for string literals. String literals
   are required to be quoted in JSON, so your consumer should look for
   a double quote, and then keep consuming characters until it sees
   the closing double quote.

   Call `lexical_error` with an appropriate message if there is
   no string literal at the beginning of the given character list.

   Also call `lexical_error` if the string literal at the beginning of
   the list has no closing double quote.

   Hint: After accepting the opening double quote, use a helper
         function to recursively look for the closing double quote,
         returning the string of characters in between, and the remainder.
*)
fun consume_string_literal (cs : char list) : string * char list =
  (* TODO, about 11 lines *) raise Fail "consume_string_literal unimplemented"


(* Challenge Problem C2: Write a consumer for the keywords true, false, and null.

   Call `lexical_error` with an appropriate message if the given
   character list does not start with a keyword.

   Hint: One way to do this is to use pattern matching to look for
         the sequence of characters for each keyword.

   Hint: Another (in our opinion, cleaner) way is to write a helper
         consumer that looks for an *arbitrary* sequence of alphabetic
         characters, and then to use `assoc` from the homework assignment
         to convert the string to a token using a lookup table such as
             [("true", TrueTok), ("false", FalseTok), ("null", NullTok)].

         (This takes advantage of the fact that assoc has a relatively
         general type, allowing the second components of the pairs to have
         *any* type whatsoever.)

         Remember that assoc returns an option -- report a lexical error as 
         appropriate.

         You can check whether a character `c` is alphabetic using the
         standard library function
             Char.isAlpha : char -> bool

   Either of the above strategies will receive full credit.
*)
fun consume_keyword (cs : char list) : token * char list =
  (* TODO, about 15 lines to do it the "clean" way, or about 5 lines to do it the "ugly" way. *)
  raise Fail "consume_keyword unimplemented"


(* Here's a provided consumer for numbers, since it's a bit complex.
   You shouldn't need to understand this code unless you want to.

   JSON uses a fairly common textual format for floating point numbers.
   The format described in Section 6 of RFC 7159, which we summarize here.

   A number consists of an optional minus sign, followed by an integer
   part, followed by an optional fractional part, followed by an
   optional exponent part.

       number = [ '-' ] int [ frac ] [ exp ]

   where single quotes enclose literal characters and square brackets
   denote optional components.

   The integer part is a nonempty sequence of digits, which may only
   start with a 0 if the entire integer part is 0.

       int = '0'
           | nonzero-digit digit*

       nonzero-digit = '1' | '2' | ... | '9'

       digit = '0' | nonzero-digit

   where vertical bars (|) denote alternatives and stars ( * ) denote
   0-or-more repetitions.

   The fractional part is a period followed by one or more digits.

       frac = '.' digit+

   where plus (+) denotes 1-or-more repetitions.

   The exponent part is a letter E (lowercase or uppercase), followed
   by an optional sign (minus or plus), followed by one or more
   digits.

       exp = e [ sign ] digit+

       e = 'e' | 'E'

       sign = '-' | '+'

   We structure consume_num in terms of several "helper consumers"
   that consume the various parts described above, such as the
   optional leading minus sign, or the fractional part, and so on.
*)
fun consume_num (cs : char list) : (string * char list) =
  let
    (* Consume an optional minus sign. We support "-" or "~" for
       compatibility with SML's Real.toString. *)
    fun consume_minus (#"-" :: cs) = ([#"-"], cs)
      | consume_minus (#"~" :: cs) = ([#"~"], cs)
      | consume_minus cs = ([], cs)

    fun consume_exp_sign (#"-" :: cs) = ([#"-"], cs)
      | consume_exp_sign (#"+" :: cs) = ([#"+"], cs)
      | consume_exp_sign cs = ([], cs)

    (* Consume a possibly empty list of digits. *)
    fun consume_digit_list cs =
      let
        fun loop acc [] = (List.rev acc, [])
          | loop acc (c :: cs) =
            if Char.isDigit c
            then loop (c :: acc) cs
            else (List.rev acc, c :: cs)
      in
        loop [] cs
      end

    fun consume_frac (#"." :: cs) =
      let
        val (l, cs) = consume_digit_list cs
      in
        (#"." :: l, cs)
      end
      | consume_frac cs = ([], cs)

    fun consume_exp (c :: cs) =
      if c = #"e" orelse c = #"E"
      then
        let
          val (sign, cs) = consume_exp_sign cs
          val (l, cs) = consume_digit_list cs
        in
          (c :: sign @ l, cs)
        end
      else ([], c :: cs)
      | consume_exp [] = ([], [])

    val (minus, cs) = consume_minus cs
    val (int,   cs) = consume_digit_list cs
    val (frac,  cs) = consume_frac cs
    val (exp,   cs) = consume_exp cs
  in
    (String.implode (minus @ int @ frac @ exp), cs)
  end


(* We now have all the consumers we need to write the main tokenizer loop. *)

(* Challenge Problem C3: Complete the following definition of `tokenize_char_list`
   that implements the tokenizer.

   Call `lexical_error` with an appropriate message if you encounter an
   unexpected character. (Use Char.toString to get a printed
   representation of a character.)

   Hint: You'll need to have one branch per kind of token, plus a few
         more to skip whitespace.

   Hint: Use the consumers from above.

   Hint: Remember to look for whitespace so that you can correctly ignore it.
 *)
fun tokenize_char_list (cs : char list) : token list =
  case cs of
     [] => []
   | #"\n" :: cs => tokenize_char_list cs (* ignore newlines *)
   | #"{" :: cs => LBrace :: tokenize_char_list cs
   (* TODO, about 7 lines: several more cases here. *)
   | c :: cs =>
     if Char.isDigit c orelse c = #"-" orelse c = #"~"
     then
         let
             val (s, cs) = consume_num (c :: cs)
         in
             NumLit s :: tokenize_char_list cs
         end
             (* TODO, about 15 lines: check for string literals and keywords here *)
     else lexical_error ("Unknown character " ^ Char.toString c)


(* Challenge Problem C4: Write the top level tokenizer that takes a string,
   converts it to a char list, and passes it to `tokenize_char_list`.

   Hint: use String.explode and tokenize_char_list *)
fun tokenize (s : string) : token list =
  (* TODO, 1 line *) raise Fail "tokenize unimplemented"

(* The tokenizer produces a list of tokens, which we now need to
   actually parse into a json value. We will structure our parser in a
   very similar way to the "consumers" used above in the tokenizer,
   except that parsers will work on token lists instead of char lists.

   For example, to parse a foo, we will write a function
       parse_foo : token list -> foo * token list
   which examines the beginning of the token list and converts it to a foo,
   returning the remainder of the list.

   As a very simple (but still useful below) example, we can write a
   parser that consumes a string literal at the beginning of the token
   list. *)

(* First, here's a provided function to report a syntax error. It takes
   the current token list and a message, and raises an exception. It
   uses the token list to print out the current token (or "EOF"
   standing for "end of file" if there are no tokens left), which
   helps when debugging to know where in the token list the error
   occurred. *)
fun syntax_error (ts : token list, msg : string) =
  let
    val tokenName =
      case ts of
         [] => "EOF"
       | t :: _ => token_to_string t
  in
    raise Fail ("Syntax error at " ^ tokenName ^ ": " ^ msg)
  end


(* Challenge Problem C5: write a `parse_string` function that consumes a string
   literal at the beginning of the token list and returns it.

   If there is no string literal at the beginning of the token list,
   call `syntax_error` with the token list and an appropriate message.
*)
fun parse_string (ts : token list) : string * token list =
  (* TODO, about 3 lines *) raise Fail "parse_string unimplemented"


(* It is often useful to consume a single token from the token list
   and throw it away, returning the rest of the tokens and throwing an
   error if the token was not there. *)

(* Challenge Problem C6: write a function `expect` which consumes a single,
   specific token from the token list.

   If the token is not there as expected, call syntax_error with an
   appropriate message. *)
fun expect (t : token, ts : token list) : token list =
  (* TODO, about 6 lines *) raise Fail "expect unimplemented"


(* We're now ready to start writing a `parse_json` function, which
   will contain several local helper functions. In this case, it is
   important that these helper functions be local and not at the top
   level, because they need to recursively call `parse_json`.

   This also makes these functions much more difficult to test, since
   they are not available at the top level scope. (There are ways
   around this, eg, using mutual recursion instead of nested
   functions.) So you'll just need to test `parse_json` extra
   thoroughly to ensure each local helper function is working properly.

   You may want to skip the helper function problems at first and
   work on the main body of parse_json (after the let...in), where
   you can parse everything except objects and arrays, and then come
   back to the helper functions later to complete the function.
*)

fun parse_json (ts : token list) : json * token list =
  let
    (* Challenge Problem C7: write a `parse_field_value` function that parses one
       field-value pair in an object.

       The syntax for a field-value pair is a string literal,
       representing the field name, followed by a colon, followed by
       an arbitrary json value.

       Hint: use `parse_string` for the field name, `expect` for the
             colon, and a recursive call to `parse_json` for the value. *)
    fun parse_field_value (ts : token list) : (string * json) * token list =
      (* TODO, about 7 lines *) raise Fail "parse_field_value unimplemented"


    (* Challenge Problem C8: write a function `parse_field_value_list` that
       parses a possibly empty comma-separated list of field-value
       pairs, terminated by a closing brace. (This will be used below
       to parse strings representing objects, which are always
       surrounded in braces.)

       Hint: use parse_field_value to parse each field-value pair.

       Hint: First check to see if the first token is a closing
             brace. If so, immediately return the empty list.
             Otherwise, parse a field-value pair and then check
             whether the next token is a comma. If so, consume it an
             recursively parse a list of field-value pairs, and then
             cons the new field-value pair on the front. If it is not a comma,
             immediately return a singleton list.
     *)
    fun parse_field_value_list (ts : token list) : (string * json) list * token list =
      (* TODO, about 15 lines *) raise Fail "parse_field_value_list unimplemented"


    (* Challenge Problem C9: Write a function `parse_array_element_list` that
       parses a possibly empty comma-separated list of json values,
       terminated by a closing square bracket.

       Hint: this is very similar to `parse_field_value_list`, except
             that it calls `parse_json` instead of
             `parse_field_value`, and uses square brackets instead of
             curly braces.
     *)
    fun parse_array_element_list (ts : token list) : json list * token list =
      (* TODO, about 15 lines *) raise Fail "parse_array_element_list unimplemented"

  in
    (* Challenge Problem C10: complete the definition of `parse_json` by adding
       branches to the pattern match below.

       If the beginning of the token list does not represent a json value,
       call `syntax_error` with an appropriate message.

       Hint: Very little new code needs to be written in each branch.
             Call the helper functions above as appropriate.
     *)
    case ts of
       NumLit s :: ts =>
       let
         val SOME r = Real.fromString s
       in
         (Num r, ts)
       end
     (* TODO, about 18 lines: more cases here *)

     | _ => syntax_error (ts, "expecting json")

  end

(* Here is a freebie function to parse a .json file. Give it a
   file name (eg, "small_police.json") *)
fun parse_from_file (file_name : string) : json =
  let
    val file = TextIO.openIn file_name
    val input = TextIO.inputAll file
    val (j, []) = parse_json (tokenize input)
  in
    j
  end

(* Make SML print a little less while we load a bunch of data. *)
       ; (* this semicolon is important because it tells SML
            that the previous binding has finished *)
Control.Print.printDepth := 3;
Control.Print.printLength := 3;

(* Uncomment once parser is implemented *)
(*
val small_example  = parse_from_file "small_police.json"
val medium_example = parse_from_file "medium_police.json"
val large_example  = parse_from_file "large_police.json"
val all_data       = parse_from_file "complete_police.json"
*)
;

(* Now make SML print more so that we can see what we're working with. *)
Control.Print.printDepth := 20;
Control.Print.printLength := 20;


(* Open-ended challenge problem C11: use your parser to explore some other
   data than the ones we have provided. If you find something
   interesting, briefly write it up.

   One thing you may run into is that your parser does not handle all
   of JSON, so it may fail to parse some files. For example, we have
   not implemented support for escape sequences inside of string
   literals, which are fairly frequently used.

   If you run into any other incompatabilities, please mention them
   here, and for extra special bonus points, fix them. *)

(* Challenge Problem C12: implement parser support for escape sequences
   such as \\, \", and \n inside of string literals. This should
   require only changing consume_string_literal. The full list of
   string escapes in JSON can be found in Section 7 of RFC 7159.

   Also implement support for *printing* strings with escape
   sequences. This should require changing only quote_string. *)
