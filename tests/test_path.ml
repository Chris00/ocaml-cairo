
open Format
open Cairo

let print_path ppf p =
  fprintf ppf "@[<2>[|";
  for i = 0 to Array.length p - 1 do
    if i > 0 then fprintf ppf ";@ ";
    match p.(i) with
    | MOVE_TO(x,y) -> fprintf ppf "MOVE_TO(%g,%g)" x y
    | LINE_TO(x,y) -> fprintf ppf "LINE_TO(%g,%g)" x y
    | CURVE_TO (x1,y1, x2,y2, x3,y3) ->
       fprintf ppf "CURVE_TO(%g,%g, %g,%g, %g,%g)" x1 y1 x2 y2 x3 y3
    | CLOSE_PATH -> fprintf ppf "CLOSE_PATH"
  done;
  fprintf ppf "@]|]"

let () =
  let tmp = Filename.get_temp_dir_name() in
  let surface = Cairo.PDF.create (Filename.concat tmp "test_path.pdf")
                  ~w:300. ~h:300. in
  let cr = Cairo.create surface in

  move_to cr 0. 0.;
  line_to cr 100. 100.;
  let p = Path.to_array (Path.copy cr) in
  printf "Current path: %a\n%!" print_path p;
  let p_rev = Path.fold (Path.copy cr) (fun l x -> x :: l) [] in
  assert(Array.to_list p = List.rev p_rev);
  let q = [| LINE_TO(110., 200.); LINE_TO(50., 150.) |] in
  Path.append cr (Path.of_array q);

  assert(Path.to_array (Path.copy cr) = Array.append p q);

  Cairo.stroke cr;
  Cairo.Surface.finish surface
