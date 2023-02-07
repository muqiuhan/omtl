type 'a test_suit = string * 'a test_case list
and 'a test_case = string * 'a
val ( +:> ) : string -> 'a test_case list -> string * 'a test_case list
val ( >== ) : string -> 'a -> 'a test_case
val fail : string -> 'a
val test_case : (unit -> unit) test_case -> string
val test_suit : (unit -> unit) test_suit -> unit
val run : (unit -> unit) test_suit -> unit
