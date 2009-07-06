(*						  -*- coding:utf-8 -*-  *)

open Printf

let l1 = 1.
let l2 = 0.5
let words = [
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
  (l1, "Mons");
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
    let canvas = { Cairo.x=20.; y=20.; w=600.; h=400. } in

    let surface = Cairo.PDF.create "cloud.pdf" 640 440 in
    let cr = Cairo.create surface in
    Random.self_init();
    Cairo.select_font_face cr font;
    Cloud.make cr canvas words ~rotate:0.1;

    Cairo.Surface.finish surface
  with Cairo.Error st as e ->
    printf "Error: %s\n" (Cairo.status_to_string st);
    raise e
