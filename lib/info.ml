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

module type Info_Impl = sig
  val get_info : unit -> string
  val filter : string list -> string list
  val decorate : string list -> string list
end

module type Info_API = sig
  val get : unit -> string
end

module Info_Generator =
functor
  (M : Info_Impl)
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

module Get_Info = struct
  module Backtrace = struct
    let get_info () = Printexc.get_raw_backtrace () |> Printexc.raw_backtrace_to_string
  end

  module CallStack = struct
    let get_info () = Printexc.get_callstack 20 |> Printexc.raw_backtrace_to_string
  end
end

module Filter = struct
  module Backtrace = struct
    let filter =
      List.filter (fun s ->
        (not (String.starts_with ~prefix:"Called from Omtl.test.time" s))
        && not (String.equal s ""))
    ;;
  end

  module CallStack = struct
    let filter (lst : string list) : string list =
      let index = ref 8 in
      let lst_len = List.length lst - !index in
      lst
      
      |> List.filter (fun _ ->
           index := !index + 1;
           !index != lst_len)
    ;;
  end
end

module Backtrace : Info_API = Info_Generator ((
  struct
    include Get_Info.Backtrace
    include Filter.Backtrace

    let decorate (lst : string list) : string list =
      match lst with
      | [] -> []
      | x :: xs -> ("| " ^ x) :: (List.map (fun x -> "                   | " ^ x)) xs
    ;;
  end :
    Info_Impl))

module CallStack : Info_API = Info_Generator ((
  struct
    include Get_Info.CallStack
    include Filter.CallStack

    let decorate (lst : string list) : string list =
      match lst with
      | [] -> []
      | x :: xs -> ("| " ^ x) :: (List.map (fun x -> "                   | " ^ x)) xs
    ;;
  end :
    Info_Impl))

module Color = struct
  module Backtrace : Info_API = Info_Generator ((
    struct
      include Get_Info.Backtrace
      include Filter.Backtrace

      let decorate (lst : string list) : string list =
        match lst with
        | [] -> []
        | x :: xs ->
          ("\027[33m| " ^ x ^ "\027[0m")
          :: (List.map (fun x -> "                   \027[37m| " ^ x ^ "\027[0m")) xs
      ;;
    end :
      Info_Impl))

  module CallStack : Info_API = Info_Generator ((
    struct
      include Get_Info.CallStack
      include Filter.CallStack

      let decorate (lst : string list) : string list =
        match lst with
        | [] -> []
        | x :: xs ->
          ("\027[33m| " ^ x ^ "\027[0m")
          :: (List.map (fun x -> "                   \027[37m| " ^ x ^ "\027[0m")) xs
      ;;
    end :
      Info_Impl))
end
