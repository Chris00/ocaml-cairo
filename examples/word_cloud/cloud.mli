
open Cairo

val make : context -> rectangle -> ?rotate:float -> ?padding:float ->
  size:('a -> string -> float) ->
  color:('a -> string -> float * float * float * float) ->
  ('a * string) list -> unit



module Palette :
sig
  type t = (float * float * float * float) array

  val random : t -> float * float * float * float

  val mauve : t
  val metal_blue : t
  val blue_green : t
  val brown : t
  val rainbow : t
  val winter : t
  val heat : t
  val blue_yellow : t
  val clay : t
  val gray : t
  val light_gray : t
end
