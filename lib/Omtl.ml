type 'a test_result =
  | Ok of 'a
  | Fail of string

type 'a test_suit = string * 'a test_case list
and 'a test_case = string * 'a

let ( +:> ) (name : string) (test_case_list : 'a test_case list) = name, test_case_list
let ( >== ) (name : string) (f : 'a) : 'a test_case = name, f

(** Wrapper function to failwith *)
let fail = failwith

let backtraces () : string =
  let backtraces : string =
    Printexc.get_raw_backtrace ()
    |> Printexc.raw_backtrace_to_string
    |> String.split_on_char '\n'
    |> List.map (( ^ ) "\t")
    |> String.concat "\n"
  in
  Format.sprintf "%s" backtraces
;;

let callstack () : string =
  let callstack : string =
    Printexc.get_callstack 20
    |> Printexc.raw_backtrace_to_string
    |> String.split_on_char '\n'
    |> List.map (( ^ ) "\t")
    |> String.concat "\n"
  in
  Format.sprintf "%s" callstack
;;

(** Returns the time the [f] took to run and the [f] execution result.
    ['a] is [f] signature
    ['b] is [f] result *)
let time (f : 'a) : float * 'b =
  let timer : float = Unix.gettimeofday () in
  try
    let result = f () in
    Unix.gettimeofday () -. timer, Ok result
  with
  | Failure s -> timer, Fail s
  | _ -> timer, Fail (callstack ())
;;

let test_case (test_case : 'a test_case) : string =
  let name, f = test_case in
  let time, result = time f in
  match result with
  | Ok _ ->
    Format.sprintf
      "   \027[32mo\027[0m- %s...\027[32mOK\027[0m \027[37m(%fs)\027[0m"
      name
      time
  | Fail s ->
    Format.sprintf
      "   \027[31mo\027[0m- %s...\027[31mFAIL\027[0m \027[37m(0s)\027[0m\n\
      \        \027[31m|!| %s\027[0m"
      name
      s
;;

let test_suit (test_suit : 'a test_suit) : unit =
  let name, test_case_list = test_suit in
  Format.sprintf "\027[35m|-\027[0m \027[37mTest suit for\027[0m \027[34m%s\027[0m" name
  |> print_endline;
  List.iter (fun case -> test_case case |> print_endline) test_case_list
;;

let run = test_suit
