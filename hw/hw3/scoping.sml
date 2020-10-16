val x = 7			(*[("x", 7)]*)
val y = 8			(*[("x", 7), ("y": 9)]*)

(*free variable is y, since its not passed in*)
(*[("x", 7), ("y": 9), ("foo", <body>)]*)
fun foo z x = z + x + y

(*[("x", 7), ("y": 10), ("foo", <body>)]*)
val y = 10

(*(*[("x", 7), ("y": 9), ("foo", <body>)]*)*)
(foo 1 1) = 1 + 1 + 10 = 12

