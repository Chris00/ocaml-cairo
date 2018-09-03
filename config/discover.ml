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
    { P.cflags = ["-I"; "C:\\gtk\\include\\cairo"];
      libs = ["/LC:\\gtk\\lib"; "cairo.lib"] }
  else { P.cflags = ["-I/usr/include/cairo"];  libs = ["-lcairo"] }

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
