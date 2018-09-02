open Cairo

let pi2 = 8. *. atan 1.

let draw cr width height =
  let r = 0.25 *. width in
  set_source_rgba cr 0. 1. 0. 0.5;
  arc cr (0.5 *. width) (0.35 *. height) ~r ~a1:0. ~a2:pi2;
  fill cr;
  set_source_rgba cr 1. 0. 0. 0.5;
  arc cr (0.35 *. width) (0.65 *. height) ~r ~a1:0. ~a2:pi2;
  fill cr;
  set_source_rgba cr 0. 0. 1. 0.5;
  arc cr (0.65 *. width) (0.65 *. height) ~r ~a1:0. ~a2:pi2;
  fill cr;
;;

let expose drawing_area _ev =
  let cr = Cairo_gtk.create drawing_area#misc#window in
  let allocation = drawing_area#misc#allocation in
  draw cr (float allocation.Gtk.width) (float allocation.Gtk.height);
  true

let () =
  ignore(GMain.init());

  let w = GWindow.window ~title:"Gtk demo" ~width:500 ~height:400 () in
  ignore(w#connect#destroy ~callback:GMain.quit);

  let d = GMisc.drawing_area ~packing:w#add () in
  ignore(d#event#connect#expose ~callback:(expose d));

  w#show();
  GMain.main()
