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

module Info_Generator =
functor
  (M : sig
     val get_info : unit -> string
     val filter : string list -> string list
     val decorate : string list -> string list
   end)
  ->
  struct
    let get () : string =
      M.get_info ()
      |> String.split_on_char '\n'
      |> M.filter
      |> M.decorate
      |> String.concat "\n"
    ;;
  end

module type Info = sig
  val get : unit -> string
end

module Backtrace : Info = Info_Generator (struct
  let decorate (lst : string list) : string list =
    match lst with
    | [] -> []
    | x :: xs -> ("| " ^ x) :: (List.map (fun x -> "                   | " ^ x)) xs
  ;;

  let filter =
    List.filter (fun s ->
      (not (String.starts_with ~prefix:"Called from Omtl.test.time" s))
      && not (String.equal s ""))
  ;;

  let get_info () = Printexc.get_raw_backtrace () |> Printexc.raw_backtrace_to_string
end)

module CallStack : Info = Info_Generator (struct
  let filter (lst : string list) : string list =
    let rec loop (lst : string list) (index : int) : string list =
      match lst with
      | [] -> []
      | x :: xs -> if index = 0 then x :: xs else loop xs (index - 1)
    in
    loop (loop lst 2 |> List.rev) 1
  ;;

  let decorate (lst : string list) : string list =
    match lst with
    | [] -> []
    | x :: xs -> ("| " ^ x) :: (List.map (fun x -> "                   | " ^ x)) xs
  ;;

  let get_info () = Printexc.get_callstack 20 |> Printexc.raw_backtrace_to_string
end)
