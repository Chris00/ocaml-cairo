open Stdio
open Printf
module C = Configurator
module P = Configurator.Pkg_config

let split_ws str =
  Base.(String.(split str ~on:' ' |> List.filter ~f:((<>) "")))

let write ~cflags ~libs =
  let write_sexp file sexp =
    Out_channel.write_all file ~data:(Base.Sexp.to_string sexp) in
  write_sexp "c_flags.sexp" (Base.sexp_of_list Base.sexp_of_string cflags);
  write_sexp "c_library_flags.sexp" (Base.sexp_of_list Base.sexp_of_string libs)

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
    match Caml.Sys.getenv "CAIRO_CFLAGS" with
    | exception Not_found -> p.P.cflags
    | alt_cflags -> split_ws alt_cflags in
  let libs =
    match Caml.Sys.getenv "CAIRO_LIBS" with
    | exception Not_found -> p.P.libs
    | alt_libs -> split_ws alt_libs in
  (* FIXME: find cairo.h and check that
     CAIRO_VERSION_MAJOR > 1 ||
     (CAIRO_VERSION_MAJOR = 1 && CAIRO_VERSION_MINOR >= 6) *)
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
    match Caml.Sys.getenv "GTK_CFLAGS" with
    | exception Not_found -> p.P.cflags
    | alt_cflags -> split_ws alt_cflags in
  let libs =
    match Caml.Sys.getenv "GTK_LIBS" with
    | exception Not_found -> p.P.libs
    | alt_libs -> split_ws alt_libs in
  write ~cflags ~libs

let () =
  let gtk = ref false in
  let specs = [
      ("--gtk", Caml.Arg.Set gtk, " add flags for Gtk")] in
  Caml.Arg.parse specs (fun _ -> raise(Caml.Arg.Bad "no anonymous arg"))
    "discover";
  Configurator.main ~name:"cairo"
    (if !gtk then discover_gtk else discover_cairo)
