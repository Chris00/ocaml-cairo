
open Cairo

type rgba = float * float * float * float

exception Failure

val make : context -> rectangle -> ?rotate:float -> ?padding:float ->
  ?word_box:(float -> rgba -> rectangle -> string -> unit) ->
  size:('a -> string -> float) -> ?min_size:float ->
  color:('a -> string -> rgba) ->
  ('a * string) list -> unit
  (** [make cr canvas size color words] make a cloud of the [words] in
      the rectangle [canvas] on the surface hold by [cr].  [size] and
      [color] must resp. return the text size and color for a given
      word.

      [word_box sz rgba r word] is executed once for each [word] where
      [sz] is the font size, [rgba] is the color of the word, and [r]
      is the rectangle reserved for that word.  This allows, for
      example, to generate an image map for the cloud. *)


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
