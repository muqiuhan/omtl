module Backtraces = struct
  type t = string

  let get () : t =
    let decorate (lst : t list) : t list =
      match lst with
      | [] -> []
      | x :: xs ->
        ("\027[33m| " ^ x ^ "\027[0m")
        :: (List.map (fun x -> "                   \027[37m| " ^ x ^ "\027[0m")) xs
    in
    Printexc.get_raw_backtrace ()
    |> Printexc.raw_backtrace_to_string
    |> String.split_on_char '\n'
    |> List.filter (fun s ->
         (not (String.starts_with ~prefix:"Called from Omtl.test.time" s))
         && not (String.equal s ""))
    |> decorate
    |> String.concat "\n"
  ;;
end

module Callstack = struct
  type t = string

  let get () : t =
    let filter (lst : t list) : t list =
      let rec loop (lst : t list) (index : int) : t list =
        match lst with
        | [] -> []
        | x :: xs -> if index = 0 then x :: xs else loop xs (index - 1)
      in
      loop (loop lst 2 |> List.rev) 1
    in
    let decorate (lst : t list) : t list =
      match lst with
      | [] -> []
      | x :: xs ->
        ("\027[33m| " ^ x ^ "\027[0m")
        :: (List.map (fun x -> "                   \027[37m| " ^ x ^ "\027[0m")) xs
    in
    Printexc.get_callstack 20
    |> Printexc.raw_backtrace_to_string
    |> String.split_on_char '\n'
    |> filter
    |> decorate
    |> String.concat "\n"
  ;;
end