
let expose drawing_area _ev =
  Printf.eprintf "Expose callback run\n%!";
  let cr = Cairo_gtk.create drawing_area#misc#window in
  Gc.major();
  Cairo.arc cr 150. 150. ~r:100. ~a1:0. ~a2:6.;
  Cairo.fill cr;
  true

let () =
  ignore(GMain.init());

  let w = GWindow.window ~title:"Gtk demo" ~width:500 ~height:400 () in
  ignore(w#connect#destroy ~callback:GMain.quit);

  let d = GMisc.drawing_area ~packing:w#add () in
  ignore(d#event#connect#expose ~callback:(expose d));

  w#show();
  GMain.main()
