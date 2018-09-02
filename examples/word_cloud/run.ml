(*						  -*- coding:utf-8 -*-  *)

open Printf
open Cairo
module Palette = Cloud.Palette

let l1 = 1.
let l2 = 0.7
let words = [
  (1.05, "Mons");
  (l1, "Psychologie");
  (l1, "Sciences de l'éducation");
  (l1, "Sciences Humaines et Sociales");
  (l1, "Sciences du langage");
  (l1, "Gestion");
  (l1, "Warocqué");
  (l1, "Sciences");
  (l1, "Polytech");
  (l1, "Ingénieur civil");
  (l1, "FTI-EII");
  (l1, "Droit");
  (l1, "Médecine");
  (l1, "Pharmacie");
  (l1, "Archi Mons");
  (l1, "Charleroi");
  l2, "Logopédie";
  l2, "Sciences politiques et sociales";
  l2, "Mathématique";
  l2, "Informatique";
  l2, "Informatique de gestion";
  l2, "Chimie";
  l2, "Physique";
  l2, "Biologie";
  l2, "Sciences biomédicales";
  l2, "Traduction";
  l2, "Interprétation";
  l2, "Architecture";
  l2, "Électricité";
  l2, "Mines et Géologie";
  l2, "Mécanique";
  l2, "Horaire décalé";
  l2, "Formation permanente";
]

let () =
  let font = try Sys.argv.(1) with _ -> "Sans" in
  try
    let canvas = { Cairo.x=50.; y=50.; w=500.; h=300. } in

    let cr = Cairo.create (Cairo.PDF.create "cloud.pdf" ~w:600. ~h:400.) in
    Random.self_init();
    Cairo.select_font_face cr font;

    let size0 = 80. in
    let size fq word =
      let te = text_extents cr word in
      size0 /. sqrt te.width  /. (1.4 -. fq) in
    let color _fq _ = Palette.random Palette.rainbow  in

    (* Show canvas *)
    save cr;
    let ux, uy = Cairo.device_to_user_distance cr 1. 1. in
    set_line_width cr (max ux uy);
    set_dash cr [| 5. |];
    rectangle cr canvas.x canvas.y ~w:canvas.w ~h:canvas.h;  stroke cr;
    restore cr;
    Cloud.make cr canvas words ~size ~color ~rotate:0.1;

    Cairo.Surface.finish (get_target cr)
  with Cairo.Error st as e ->
    printf "Fatal error: %s\n" (Cairo.status_to_string st);
    raise e
