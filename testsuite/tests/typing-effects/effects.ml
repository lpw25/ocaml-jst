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
