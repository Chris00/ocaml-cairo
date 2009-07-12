
open Cairo

val make : context -> rectangle -> ?rotate:float ->
  size:('a -> string -> float) ->
  color:('a -> string -> float * float * float * float) ->
  ('a * string) list -> unit
