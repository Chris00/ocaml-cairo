(* Based on https://cairographics.org/cookbook/pycairo_pango/ *)


let () =
  let surface = Cairo.Image.(create ARGB32 ~w:320 ~h:120) in
  let cr = Cairo.create surface in
  (* Draw a background rectangle: *)
  Cairo.rectangle cr 0. 0. ~w:320. ~h:120.;
  Cairo.set_source_rgb cr 1. 1. 1.;
  Cairo.fill cr;
  (* Get font families: *)
  let font_map = Cairo_pango.Font_map.get_default() in

  Cairo.translate cr 50. 25.;
  let pc = Cairo_pango.Font_map.create_context font_map in
  let layout = Pango.Layout.create pc in
  let fontname = if Array.length Sys.argv >= 2 then Sys.argv.(1) else "Sans" in
  let font = Pango.Font.from_string fontname in
  Pango.Layout.set_font_description layout font;
  Pango.Layout.set_text layout "Hello world こんにちは世界";
  Cairo.set_source_rgb cr 0. 0. 0.;
  Cairo_pango.update_layout cr layout;
  Cairo_pango.show_layout cr layout;

  Cairo.PNG.write surface "rendering.png"
