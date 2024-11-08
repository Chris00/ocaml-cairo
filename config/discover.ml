module C = Configurator.V1
module P = C.Pkg_config

let write ~cflags ~libs =
  C.Flags.write_sexp "c_flags.sexp" cflags;
  C.Flags.write_sexp "c_library_flags.sexp" libs
(* let write ~cflags:_ ~libs:_ = () *)

let default_cairo c =
  (* In case pkg-config fails *)
  let sys = C.ocaml_config_var_exn c "system" in
  if sys = "msvc" || sys = "win64" then
    { P.cflags = ["-I"; "C:\\gtk\\include\\cairo";
                  "-I"; "C:\\gtk\\include\\freetype2"];
      libs = ["/LC:\\gtk\\lib"; "cairo.lib"] }
  else { P.cflags = ["-I/usr/include/cairo"];  libs = ["-lcairo"] }

let c_header_has_ft () =
  let fh = open_in "cairo_ocaml.h.p" in
  let buf = Buffer.create 4096 in
  let b = Bytes.create 4096 in
  let n = ref 0 in
  while n := input fh b 0 4096;  !n > 0 do
    Buffer.add_subbytes buf b 0 !n
  done;
  close_in fh;
  let s = Buffer.contents buf in
  let re = Str.regexp "/\\* *#define *OCAML_CAIRO_HAS_FT .*\\*/" in
  let s = Str.global_replace re "#define OCAML_CAIRO_HAS_FT 1" s in
  let fh = open_out "cairo_ocaml.h" in
  output_string fh s;
  close_out fh

let discover_cairo c =
  let p = match P.get c with
    | Some p -> (match P.query p ~package:"cairo" with
                 | Some p -> p | None -> default_cairo c)
    | None -> default_cairo c in
  let cflags =
    match Sys.getenv "CAIRO_CFLAGS" with
    | exception Not_found -> p.P.cflags
    | alt_cflags -> C.Flags.extract_blank_separated_words alt_cflags in
  let libs =
    match Sys.getenv "CAIRO_LIBS" with
    | exception Not_found -> p.P.libs
    | alt_libs -> C.Flags.extract_blank_separated_words alt_libs in
  (* Check Cairo version *)
  let d = C.C_define.(import c ~includes:["cairo.h"] ~c_flags:cflags
                        ["CAIRO_VERSION_MAJOR", Type.Int;
                         "CAIRO_VERSION_MINOR", Type.Int ]) in
  let version_major = match List.assoc "CAIRO_VERSION_MAJOR" d with
    | C.C_define.Value.Int d -> d
    | _ -> assert false in
  let version_minor = match List.assoc "CAIRO_VERSION_MINOR" d with
    | C.C_define.Value.Int d -> d
    | _ -> assert false in
  if not(version_major > 1 || (version_major = 1 && version_minor >= 6)) then
    C.die "Cairo version us %d.%02d but must be at least 1.06\n"
      version_major version_minor;
  (*
   * Add fontconfig flags and libs if available in Cairo.
   *)
  let d = C.C_define.(import c ~includes:["cairo-ft.h"] ~c_flags:cflags
                        ["CAIRO_HAS_FT_FONT", Type.Switch;
                         "CAIRO_HAS_FC_FONT", Type.Switch ]) in
  let has_ft_font = match List.assoc "CAIRO_HAS_FT_FONT" d with
    | C.C_define.Value.Switch b -> b
    | _ -> false in
  let has_fc_font = match List.assoc "CAIRO_HAS_FC_FONT" d with
    | C.C_define.Value.Switch b -> b
    | _ -> false in
  let cflags, libs =
    if has_ft_font && has_fc_font then (
      match P.get c with
      | Some p ->
         (match P.query p ~package:"fontconfig freetype2" with
          | Some f ->
             (* freetype/ftmodapi.h on Debian but no prefix
                directory on Ubuntu. *)
             let freetype l f =
               if String.length f > 2 && f.[0] = '-' && f.[1] = 'I' then
                 f :: (f ^ "/freetype") :: l
               else f :: l in
             c_header_has_ft ();
             (List.fold_left freetype [] f.cflags @ cflags,
              f.libs @ libs)
          | None -> cflags, libs)
      | None -> cflags, libs
    )
    else cflags, libs in
  write ~cflags ~libs

let default_gtk c =
  let sys = C.ocaml_config_var_exn c "system" in
  if sys = "msvc" || sys = "win64" then
    { P.cflags = ["-I"; "C:\\gtk\\include"];
      libs = ["/LC:\\gtk\\lib"; "gtk.lib"] }
  else C.die "Please set Gtk flags through the environment variables \
              GTK_CFLAGS and GTK_LIBS."

let discover_gtk c =
  let p = match P.get c with
    | Some p -> (match P.query p ~package:"gtk+-2.0" with
                 | Some p -> p | None -> default_gtk c)
    | None -> default_gtk c in
  let cflags =
    match Sys.getenv "GTK_CFLAGS" with
    | exception Not_found -> p.P.cflags
    | alt_cflags -> C.Flags.extract_blank_separated_words alt_cflags in
  let libs =
    match Sys.getenv "GTK_LIBS" with
    | exception Not_found -> p.P.libs
    | alt_libs -> C.Flags.extract_blank_separated_words alt_libs in
  write ~cflags ~libs

let () =
  let gtk = ref false in
  let specs = [
      ("--gtk", Arg.Set gtk, " add flags for Gtk")] in
  Arg.parse specs (fun _ -> raise(Arg.Bad "no anonymous arg"))
    "discover";
  C.main ~name:"cairo"
    (if !gtk then discover_gtk else discover_cairo)
