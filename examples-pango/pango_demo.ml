open Cairo

let two_pi = 2. *. acos(-1.)

let radius = 150.

(* Based on the example given at
   https://developer.gnome.org/pango/stable/pango-Cairo-Rendering.html *)
let draw_text (cr: context) =
  let n_words = 10 in
  let font = "Sans Bold 26" in
  Cairo.translate cr radius radius;
  let layout = Cairo_pango.create_layout cr in
  Pango.Layout.set_text layout "Text";
  let desc = Pango.Font.from_string font in
  Pango.Layout.set_font_description layout desc;
  (* Draw the layout [n_words] times in a circle. *)
  for i = 1 to n_words do
    let angle = two_pi *. float i /. float n_words in
    Cairo.save cr;
    let red = (1. +. cos(angle -. two_pi /. 6.)) /. 2. in
    Cairo.set_source_rgb cr red 0. (1. -. red);
    Cairo.rotate cr angle;
    (* Inform Pango to re-layout the text with the new transformation. *)
    Cairo_pango.update_layout cr layout;
    let width, _height = Pango.Layout.get_size layout in
    Cairo.move_to cr (-. (float width /. float Pango.scale) /. 2.) (-. radius);
    Cairo_pango.show_layout cr layout;
    Cairo.restore cr;
  done

let () =
  let diam = truncate(2. *. radius) in
  let surface = Cairo.Image.(create ARGB32 ~w:diam ~h:diam) in
  let cr = Cairo.create surface in
  Cairo.set_source_rgb cr 1. 1. 1.;
  Cairo.paint cr;
  draw_text cr;
  Cairo.PNG.write surface "pango_demo.png"
