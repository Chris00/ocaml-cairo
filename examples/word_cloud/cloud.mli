
open Cairo

val make : context -> rectangle -> ?rotate:float ->
  size:('a -> string -> float) -> ('a * string) list -> unit


val area : float -> string -> float
