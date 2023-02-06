open Omtl

(* A module with functions to test *)
module My_String = struct
  let lowercase = String.lowercase_ascii
  let capitalize = String.capitalize_ascii
  let str_concat = String.concat ""
end

(* The tests *)
let test_lowercase () =
  if String.equal "hello!" (My_String.lowercase "hELLO!")
  then ()
  else fail "My_String.lowercase \"hELLO!\" = \"hello!\""
;;

let test_capitalize () =
  if String.equal "HELLO!" (My_String.capitalize "hELLO!")
  then ()
  else fail "My_String.capitalize \"hELLO!\" = \"HELLO!!\""
;;

let test_str_concat () =
  if String.equal "foobar" (My_String.str_concat [ "foo"; "bar" ])
  then ()
  else fail "My_String.str_concat [\"foo\"; \"bar\"] = \"foobar\""
;;

(* Run it *)
let _ =
  "My_String"
  +:> [ "lowercase" >== test_lowercase
      ; "capitalize" >== test_capitalize
      ; "str_concat" >== test_str_concat
      ; ("Examples of test failures"
        >== fun () -> fail "Take it easy, this is just an example of a failed test")
      ; ("Test function running time" >== fun () -> Unix.sleep 1)
      ]
  |> run
;;
