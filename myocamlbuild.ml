(* OASIS_START *)
(* OASIS_STOP *)

let rec split_on is_delim s i0 i i1 =
  if i >= i1 then [String.sub s i0 (i1 - i0)]
  else if is_delim s.[i] then
    String.sub s i0 (i - i0) :: skip is_delim s (i + 1) i1
  else
    split_on is_delim s i0 (i + 1) i1
and skip is_delim s i i1 =
  if i >= i1 then []
  else if is_delim s.[i] then skip is_delim s (i + 1) i1
  else split_on is_delim s i (i + 1) i1

let split_on_tab s = split_on (fun c -> c = '\t') s 0 0 (String.length s)

let env = BaseEnvLight.load() (* setup.data *)

let cflags = split_on_tab (BaseEnvLight.var_get "cairo_cflags" env)
let cclib = split_on_tab (BaseEnvLight.var_get "cairo_clibs" env)

(* Gtk *)
let lablgtk2_cflags =
  try ["-I" ^ BaseEnvLight.var_get "pkg_lablgtk2" env]
  with Not_found -> [] (* if --disable-lablgtk2 *)
let gtk_cflags =
  try split_on_tab(BaseEnvLight.var_get "gtk_cflags" env) @ lablgtk2_cflags
  with Not_found ->
       Printf.eprintf "ERROR: gtk_cflags variable not in setup.data\n"; exit 1
;;


Ocamlbuild_plugin.dispatch
  (MyOCamlbuildBase.dispatch_combine [
    dispatch_default;
    begin function
    | After_rules ->
      let includes =
        ["cairo_macros.c"; "cairo_ocaml.h"; "cairo_ocaml_types.c"] in
      let includes = List.map (fun f -> "src" / f) includes in
      dep ["c"; "compile"] includes;

      let cflags =
        List.fold_right (fun o l -> A "-ccopt" :: A o :: l) cflags [] in
      flag ["use_libcairo_c"; "compile"; "c"] (S cflags);
      flag ["use_libcairo_c"; "library"; "shared"]
        (S (A "-cclib" :: A "-Lsrc/" :: cflags));
      let cclib_spec =
        List.fold_right (fun o l -> A "-cclib" :: A o :: l) cclib [] in
      flag ["use_libcairo_c"; "link"] (S cclib_spec);
      flag ["ocamlmklib"; "c"]
        (S (List.map (fun o -> A o) cclib));
      flag ["use_libgtk_c"; "compile"; "c"]
        (S (List.fold_right (fun o l -> A "-ccopt" :: A o :: l) gtk_cflags []));

      (* ocamlbuild does not support cmxs before 3.12.0.  Quick hack. *)
      let version_major, version_minor =
        Scanf.sscanf Sys.ocaml_version "%i.%i" (fun mj mi -> mj, mi) in
      if version_major <= 3 && version_minor <= 11 then (
        rule "cmxa -> cmxs" ~dep:"%.cmxa" ~prod:"%.cmxs"
          begin fun env build ->
            let cmxa = env "%.cmxa" and cmxs = env "%.cmxs" in
            Cmd(S [!Options.ocamlopt; A "-shared"; A "-linkall";
                   T (tags_of_pathname cmxs ++ "library" ++ "native" ++ "ocaml"
                      ++ "link" ++ "shared");
                   P cmxa; A "-o"; Px cmxs])
          end;
      )

    | _ -> ()
    end;
  ]);;
