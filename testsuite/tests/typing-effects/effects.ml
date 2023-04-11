(* TEST
   flags += "-extension effects"
   * expect *)

type 'a state

let set : 'a -> unit [state: 'a state] = fun _ -> ()
[%%expect{|
type 'a state
val set : 'a -> unit [state: 'a state] = <fun>
|}]

let foo x = set x
[%%expect{|
val foo : 'a -> unit [state: 'a state] = <fun>
|}]

let () = foo 8
[%%expect{|
Line 1, characters 9-14:
1 | let () = foo 8
             ^^^^^
Error: This expression performs effects [state: int state]
       but the current effect context is []
       The second effect context has no effect state
|}]

;; foo 8.8
[%%expect{|
Line 1, characters 3-10:
1 | ;; foo 8.8
       ^^^^^^^
Error: This expression performs effects [state: float state]
       but the current effect context is []
       The second effect context has no effect state
|}]

let bar x y = foo x
[%%expect{|
val bar : 'a -> 'b -> unit [state: 'a state] = <fun>
|}]

let bar8 =
  fun () -> bar 8
[%%expect{|
Line 2, characters 12-17:
2 |   fun () -> bar 8
                ^^^^^
Error: This local value escapes its region
  Hint: Cannot return local value without an explicit "local_" annotation
|}]

let rec map (local_ f) = function
  | [] -> []
  | x :: xs -> f x :: map f xs

let bar l =
  map (fun x -> set x; x + 1) l
[%%expect{|
val map : local_ ('a -> 'b) -> 'a list -> 'b list = <fun>
val bar : int list -> int list [state: int state] = <fun>
|}]

let r : (int -> int) option ref = ref None

let bad_map f l =
  let f' =
    match !r with
    | None -> f
    | Some f' -> f'
  in
  r := Some f;
  let rec loop = function
  | [] -> []
  | x :: xs -> f' x :: loop xs
  in
  loop l [@nontail]

let bar l =
  bad_map (fun x -> set x; x + 1) l
[%%expect{|
val r : (int -> int) option ref = {contents = None}
val bad_map : (int -> int) -> int list -> int list = <fun>
Line 17, characters 10-33:
17 |   bad_map (fun x -> set x; x + 1) l
               ^^^^^^^^^^^^^^^^^^^^^^^
Error: This expresion has effects [state: int state]
       which escape the current region
|}]

let use_stateful (f : unit -> unit [state: int]) =
  f ()
[%%expect{|
val use_stateful : (unit -> unit [state: int]) -> unit [state: int] = <fun>
|}]

let handle_int_state (f : unit -> unit [state: int state]) = ()
[%%expect{|
val handle_int_state : (unit -> unit [state: int state]) -> unit = <fun>
|}]

let without_state () =
  handle_int_state (fun () -> set 8)
[%%expect{|
val without_state : unit -> unit = <fun>
|}]

let without_state () =
  handle_int_state (fun () -> set 8.8)
[%%expect{|
Line 2, characters 19-38:
2 |   handle_int_state (fun () -> set 8.8)
                       ^^^^^^^^^^^^^^^^^^^
Error: This expression has effect [state: float state]
       but an expression was expected with effect [state: int state]
       Types for effect state are incompatible
|}]

let handle_stat (f : unit -> unit [stat: _]) = ()
[%%expect{|
val handle_stat : (unit -> unit [stat: 'a]) -> unit = <fun>
|}]

let without_state () =
  handle_stat (fun () -> set 8)
[%%expect{|
Line 2, characters 14-31:
2 |   handle_stat (fun () -> set 8)
                  ^^^^^^^^^^^^^^^^^
Error: This expression has effect [state: int state]
       but an expression was expected with effect [stat: 'a]
       The first effect context has effect state
       where the second has effect stat
|}]

type foo = { foo : unit -> unit [state : int state] }
[%%expect{|
type foo = { foo : unit -> unit [state: int state]; }
|}]

let bar { foo } = foo ()
[%%expect{|
val bar : foo -> unit [state: int state] = <fun>
|}]

let _ = { foo = fun () -> set 0 }
[%%expect{|
- : foo = {foo = <fun>}
|}]

let join p =
  if p then set 0
  else set 0.0
[%%expect{|
Line 3, characters 7-14:
3 |   else set 0.0
           ^^^^^^^
Error: This expression performs effects [state: float state]
       but the current effect context is [state: int state]
       Types for effect state are incompatible
|}]

let tuple_test1 p =
  let x, y =
    if p then set, (fun _ -> ())
    else set, (fun _ -> ())
  in
  y
[%%expect{|
val tuple_test1 : bool -> 'a -> unit = <fun>
|}]

let tuple_test2 p =
  let x, y =
    if p then set, (fun _ -> ())
    else set, (fun _ -> ())
  in
  x
[%%expect{|
val tuple_test2 : bool -> 'a -> unit [state: 'a state] = <fun>
|}]

let fail msg = perform_ fail msg
[%%expect{|
val fail : 'a -> 'b [fail: 'a] = <fun>
|}]

let handle_bad (local_ f) =
  match f () with
  | x -> x
  | effect_ fail msg -> String.length msg
[%%expect{|
Line 2, characters 8-9:
2 |   match f () with
            ^
Error: The value f is local, so cannot be used inside an effect handler
|}]

let handle (f : unit -> int [fail: string]) =
  match f () with
  | x -> x
  | effect_ fail msg -> String.length msg
let five = handle (fun () -> fail "hello")
[%%expect{|
val handle : (unit -> int [fail: string]) -> int = <fun>
val five : int = 5
|}]

let handle_or1 (f : unit -> int [fail: int]) =
  match f () with
  | x | effect_ fail x -> x + 2
let seven = handle_or1 (fun () -> fail 5)
let eight = handle_or1 (fun () -> 6)
[%%expect{|
val handle_or1 : (unit -> int [fail: int]) -> int = <fun>
val seven : int = 7
val eight : int = 8
|}]

let handle_or2 (f : unit -> int [fail: string]) =
  match f () with
  | x -> "hello"
  | effect_ fail s | exception (Failure s) -> s ^ "!"
let eff = handle_or2 (fun () -> fail "eff")
let exn = handle_or2 (fun () -> failwith "exn")
[%%expect{|
val handle_or2 : (unit -> int [fail: string]) -> string = <fun>
val eff : string = "eff!"
val exn : string = "exn!"
|}]

let handle_or3 (f : unit -> string [fail: string]) =
  match f () with
  | s | effect_ fail s | exception (Failure s) -> s ^ "?"
let a = handle_or3 (fun () -> "a")
let b = handle_or3 (fun () -> fail "b")
let c = handle_or3 (fun () -> failwith "c")
[%%expect{|
val handle_or3 : (unit -> string [fail: string]) -> string = <fun>
val a : string = "a?"
val b : string = "b?"
val c : string = "c?"
|}]
