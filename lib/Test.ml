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

type test_suit = string * test_case list
and test_case = string * (unit -> unit)

module Test_Result = struct
  type t =
    | Ok of time
    | Fail of info * backtraces * callstack

  and time = float
  and info = string
  and callstack = string
  and backtraces = string
end

let test
  ?(backtrace : bool = false)
  ?(callstack : bool = false)
  ?(color : bool = false)
  (f : unit -> unit)
  : Test_Result.t
  =
  try
    let time (f : unit -> unit) : float =
      let timer : float = Unix.gettimeofday () in
      f ();
      Unix.gettimeofday () -. timer
    in
    Ok (time f)
  with
  | Failure s ->
    let backtrace =
      if backtrace then if color then Color.Backtrace.get () else Backtrace.get () else ""
    in
    let callstack =
      if callstack then if color then Color.CallStack.get () else CallStack.get () else ""
    in
    Fail (s, backtrace, callstack)
  | e -> Fail ("Exception: " ^ Printexc.to_string e, "", "")
;;

let test_case
  ?(backtrace : bool = false)
  ?(callstack : bool = false)
  ?(color : bool = false)
  (test_case : test_case)
  : string
  =
  let name, f = test_case in
  match test f ~backtrace ~callstack ~color with
  | Test_Result.Ok time ->
    if color
    then
      Format.sprintf
        "   \027[32mo\027[0m- %s...\027[32mOK\027[0m \027[38m(%fs)\027[0m"
        name
        time
    else Format.sprintf "   o- %s...OK (%fs)" name time
  | Test_Result.Fail (i, b, c) ->
    if color
    then
      Format.sprintf
        "   \027[31mo\027[0m- %s...\027[31mFAIL\027[0m \027[38m(0s)\027[0m\n\
        \        \027[31m|!| %s\027[0m\n\
         %s%s"
        name
        i
        (if backtrace
        then
          Format.sprintf
            "        \027[4;36mBACKTRACES\027[0m %s\n"
            (if String.length b = 0 then "No more info" else b)
        else "")
        (if callstack
        then
          Format.sprintf
            "        \027[4;36mCALLSTACKS\027[0m %s"
            (if String.length c = 0 then "No more info" else c)
        else "")
    else
      Format.sprintf
        "   o- %s...FAIL (0s)\n        |!| %s\n%s%s"
        name
        i
        (if backtrace
        then
          Format.sprintf
            "        BACKTRACES %s\n"
            (if String.length b = 0 then "No more info" else b)
        else "")
        (if callstack
        then
          Format.sprintf
            "        CALLSTACKS %s"
            (if String.length c = 0 then "No more info" else c)
        else "")
;;

let test_suit
  ?(backtrace : bool = false)
  ?(callstack : bool = false)
  ?(color : bool = false)
  (test_suit : test_suit)
  : unit
  =
  let name, test_case_list = test_suit in
  Format.sprintf "\027[35m|-\027[0m \027[38mTest suit for\027[0m \027[1;34m%s\027[0m" name
  |> print_endline;
  List.iter
    (fun case -> test_case case ~backtrace ~callstack ~color |> print_endline)
    test_case_list
;;

let run
  ?(backtrace : bool = false)
  ?(callstack : bool = false)
  ?(color : bool = false)
  (suit : test_suit)
  =
  if backtrace then Printexc.record_backtrace true;
  let _ = test_suit ~backtrace ~callstack ~color suit in
  ()
;;
