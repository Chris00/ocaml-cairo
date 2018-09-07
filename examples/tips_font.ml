(* This file is part of the tutorial
   http://cairo.forge.ocamlcore.org/tutorial/
*)

open Cairo

let () =
  let alphabet = "AbCdEfGhIjKlMnOpQrStUvWxYz" in
  let surface = Cairo.Image.create Cairo.Image.ARGB32 ~w:780 ~h:30 in
  let cr = Cairo.create surface in
  (* Examples are in 26.0 x 1.0 coordinate space *)
  Cairo.scale cr 30. 30.;
  Cairo.set_font_size cr 0.8;

  (* Drawing code goes here *)
  Cairo.set_source_rgb cr 0.0 0.0 0.0;
  Cairo.select_font_face cr "Georgia" ~weight:Bold;

  let fe = Cairo.font_extents cr in
  for i = 0 to String.length alphabet - 1 do
    let letter = String.make 1 (alphabet.[i]) in
    let te = Cairo.text_extents cr letter in
    Cairo.move_to cr (float i +. 0.5 -. te.x_bearing -. te.width /. 2.)
      (0.5 -. fe.descent +. fe.baseline /. 2.);
    Cairo.show_text cr letter;
  done;

  (* Write output *)
  Cairo.PNG.write surface "tips_font.png"
