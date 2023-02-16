(**********************************************************************************)
(* MIT License                                                                    *)
(*                                                                                *)
(* Copyright (c) 2023 Muqiu Han                                                   *)
(*                                                                                *)
(* Permission is hereby granted, free of charge, to any person obtaining a copy   *)
(* of this software and associated documentation files (the "Software"), to deal  *)
(* in the Software without restriction, including without limitation the rights   *)
(* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      *)
(* copies of the Software, and to permit persons to whom the Software is          *)
(* furnished to do so, subject to the following conditions:                       *)
(*                                                                                *)
(* The above copyright notice and this permission notice shall be included in all *)
(* copies or substantial portions of the Software.                                *)
(*                                                                                *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     *)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    *)
(* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         *)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  *)
(* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  *)
(* SOFTWARE.                                                                      *)
(**********************************************************************************)

open Info

(** start record backtrace *)
let _ = Printexc.record_backtrace true

type 'a test_suit = string * 'a test_case list
and 'a test_case = string * 'a

(** Assist in building test sets and individual test items to improve the readability of test code *)
let ( +:> ) (name : string) (test_case_list : 'a test_case list) = name, test_case_list

let ( >== ) (name : string) (f : 'a) : 'a test_case = name, f

(** Wrapper function to failwith *)
let fail = failwith

module Test_Result = struct
  type t =
    | Ok of time
    | Fail of info * backtraces * callstack

  and time = float
  and info = string
  and callstack = string
  and backtraces = string
end

(** Returns the time the [f] took to run and the [f] execution result.
    ['a] is [f] signature
    ['b] is [f] result *)
let test (f : 'a) : Test_Result.t =
  try
    let time (f : 'a) : float =
      let timer : float = Unix.gettimeofday () in
      f ();
      Unix.gettimeofday () -. timer
    in
    Ok (time f)
  with
  | Failure s -> Fail (s, Backtrace.get (), CallStack.get ())
  | e -> Fail ("Exception: " ^ Printexc.to_string e, Backtrace.get (), CallStack.get ())
;;

let test_case (test_case : 'a test_case) : string =
  let name, f = test_case in
  match test f with
  | Ok time ->
    Format.sprintf
      "   \027[32mo\027[0m- %s...\027[32mOK\027[0m \027[38m(%fs)\027[0m"
      name
      time
  | Fail (i, b, c) ->
    Format.sprintf
      "   \027[31mo\027[0m- %s...\027[31mFAIL\027[0m \027[38m(0s)\027[0m\n\
      \        \027[31m|!| %s\027[0m\n\
      \        \027[4;36mBACKTRACES\027[0m %s\n\
      \                   \027[37m|\027[0m\n\
      \        \027[4;36mCALLSTACKS\027[0m %s"
      name
      i
      b
      c
;;

let test_suit (test_suit : 'a test_suit) : unit =
  let name, test_case_list = test_suit in
  Format.sprintf "\027[35m|-\027[0m \027[38mTest suit for\027[0m \027[1;34m%s\027[0m" name
  |> print_endline;
  List.iter (fun case -> test_case case |> print_endline) test_case_list
;;

let run = test_suit
