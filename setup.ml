(* OASIS_START *)
(* DO NOT EDIT (digest: a426e2d026defb34183b787d31fbdcff) *)
(******************************************************************************)
(* OASIS: architecture for building OCaml libraries and applications          *)
(*                                                                            *)
(* Copyright (C) 2011-2016, Sylvain Le Gall                                   *)
(* Copyright (C) 2008-2011, OCamlCore SARL                                    *)
(*                                                                            *)
(* This library is free software; you can redistribute it and/or modify it    *)
(* under the terms of the GNU Lesser General Public License as published by   *)
(* the Free Software Foundation; either version 2.1 of the License, or (at    *)
(* your option) any later version, with the OCaml static compilation          *)
(* exception.                                                                 *)
(*                                                                            *)
(* This library is distributed in the hope that it will be useful, but        *)
(* WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY *)
(* or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more         *)
(* details.                                                                   *)
(*                                                                            *)
(* You should have received a copy of the GNU Lesser General Public License   *)
(* along with this library; if not, write to the Free Software Foundation,    *)
(* Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA              *)
(******************************************************************************)

let () =
  try
    Topdirs.dir_directory (Sys.getenv "OCAML_TOPLEVEL_PATH")
  with Not_found -> ()
;;
#use "topfind";;
#require "oasis.dynrun";;
open OASISDynRun;;

let setup_t = BaseCompat.Compat_0_4.adapt_setup_t setup_t
open BaseCompat.Compat_0_4
(* OASIS_STOP *)

open Printf;;
#load "unix.cma";;
#use "config.ml";;

let () =
  try let v = OASISExec.run_read_one_line ~ctxt:!BaseContext.default
                                          "pkg-config" ["--version"] in
      OASISMessage.info ~ctxt:!OASISContext.default
                        "Found pkg-config %s\n%!" v;
  with Failure _ ->
    OASISMessage.error ~ctxt:!OASISContext.default
                       "Please install \"pkg-config\".\n%!";
    exit 1

let pkg_config lib args =
  try
    OASISExec.run_read_one_line ~ctxt:!BaseContext.default
                                "pkg-config" (lib :: args)
  with Failure _ ->
    printf "Please install the development files for the C library %S.\n" lib;
    exit 1

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

let split_on_space s = split_on (fun c -> c = ' ') s 0 0 (String.length s)

(** Compute the correct CFLAGS for Cairo. *)
let cairo_cflags =
  lazy(match cairo_cflags with
       | [] ->
         (match Sys.os_type with
          | "Unix" -> split_on_space(pkg_config "cairo" ["--cflags-only-I"])
          | "Cygwin" -> ["-I"; "C:/gtk/include/cairo"]
          | "Win32" -> ["/I"; "C:\\gtk\\include\\cairo"]
          | os -> printf "Operating system %S not known" os;  exit 1)
       | _ -> cairo_cflags)

(** Compute the correct CLIBS for Cairo. *)
let cairo_clibs =
  lazy(match cairo_clibs with
       | [] ->
         (match Sys.os_type with
          | "Unix" -> split_on_space(pkg_config "cairo" ["--libs"])
          | "Cygwin" -> ["-LC:/gtk/lib"; "-lcairo"]
          | "Win32" -> ["/LC:\\gtk\\lib"; "cairo.lib"]
          | os -> printf "Operating system %S not known" os;  exit 1)
       | _ -> cairo_clibs)


let compile_and_run_c =
  let cc, ccargs =
    match OASISString.nsplit(BaseStandardVar.bytecomp_c_compiler()) ' ' with
    | cc :: args -> cc, args
    | _ -> printf "No C compiler detected!! Check ocamlc -config"; exit 1 in
  (* On some platforms, the OCaml C headers are not in a the locations
     searched by the C compiler. *)
  let stdlib = "-I" ^ BaseStandardVar.standard_library() in
  fun ?(cflags=[]) ?(lflags=[]) pgm ?(run_err=(fun _ -> ())) compile_err -> (
    let tmp, fh = Filename.open_temp_file "setup" ".c" in
    output_string fh pgm;
    close_out fh;
    let exe = Filename.temp_file "oasis-" ".exe" in
    let o = match Sys.os_type with
      | "Unix" | "Cygwin" -> "-o " ^ exe
      | "Win32" -> "/Fe" ^ exe
      | _ -> assert false in
    let args = o :: stdlib :: (cflags @ [tmp] @ lflags) in
    let f_exit_code e =
      if e <> 0 then (compile_err(); Sys.remove tmp; exit 1) in
    OASISExec.run ~ctxt:!BaseContext.default cc args ~f_exit_code;
    Sys.remove tmp;
    OASISExec.run ~ctxt:!BaseContext.default exe [] ~f_exit_code:run_err;
    Sys.remove exe
  )

let get_cairo_cflags () =
  let cairo_cflags = Lazy.force cairo_cflags in
  compile_and_run_c ~cflags:cairo_cflags
    "#include <cairo.h>
     int main(int argc, char **argv) {
        if(CAIRO_VERSION_MAJOR >= 1 && CAIRO_VERSION_MINOR >= 6) {
          return(0);
        } else {
          return(1);
        }
     }"
    (fun _ -> printf "ERROR: Could not compile a test program. The path to \
                   cairo headers in %S is likely incorrect.  Set it using \
                   cairo_cflags in config.ml.\n"
                  (String.concat " " cairo_cflags))
    ~run_err:(fun e ->
      if e <> 0 then printf "ERROR: Please install cairo version >= 1.6\n");
  String.concat "\t" cairo_cflags

let _ = BaseEnv.var_define "cairo_cflags" get_cairo_cflags

let get_cairo_clibs () =
  let cairo_clibs = Lazy.force cairo_clibs in
  compile_and_run_c ~cflags:(Lazy.force cairo_cflags) ~lflags:cairo_clibs
    "#include <cairo.h>
     int main(int argc, char **argv) {
       cairo_t* cr;
       cr = cairo_create(cairo_image_surface_create(CAIRO_FORMAT_RGB24, 10,10));
       cairo_destroy(cr);
       return(0);
     }"
    (fun _ -> printf "ERROR: Could not compile a test program. The cairo library \
                   flags %S are likely incorrect.  Set them in config.ml.\n"
                  (String.concat " " cairo_clibs));
  String.concat "\t" cairo_clibs

let _ = BaseEnv.var_define "cairo_clibs" get_cairo_clibs

let gtk_cflags =
  lazy(match gtk_cflags with
       | [] ->
         if BaseEnv.var_get "lablgtk2" = "true" then
           pkg_config "gtk+-2.0" ["--cflags-only-I"]
         else "<lablgtk2 disabled>"
       | _ -> String.concat "\t" gtk_cflags)

let () =
  let configure t args =
    setup_t.BaseSetup.configure t args;
    (* One must define this variable after the standard configure for
       the flag(lablgtk2) to be correctly set if --enable-lablgtk2
       was not present on the command line of configure. *)
    let _ = BaseEnv.var_define "gtk_cflags" (fun () -> Lazy.force gtk_cflags) in

    (* Checks at the end of configure for early warnings.  [gtk_cflags]
       is only known here; one cannot put this ourside [configure]. *)
    if BaseEnv.var_get "lablgtk2" = "true" then (
      (* When lablgtk2 will be used, make sure the C headers are present
         (these may be in a different package than the OCaml library). *)
      let gtk2 = BaseEnv.var_get "gtk_cflags" in
      let lablgtk2 = "-I" ^ OASISExec.run_read_one_line
                              ~ctxt:!BaseContext.default
                              (BaseCheck.ocamlfind ())
                              ["query"; "-format"; "%d"; "lablgtk2"] in
      compile_and_run_c ~cflags:[gtk2; lablgtk2]
        "#include <gtk/gtkversion.h>\n\
         #include <gdk/gdk.h>\n\
         #include <wrappers.h>\n\
         #include <ml_gobject.h>\n\
         #include <ml_gdk.h>\n\
         #include <ml_gdkpixbuf.h>\n\
         int main(int argc, char **argv) {
           return(0);
         }"
        (fun _ -> printf "ERROR: Install the development files for lablgtk2 \
                          (e.g. the\n  liblablgtk2-ocaml-dev package for \
                          Debian).\n");
    ) in
  BaseSetup.setup { setup_t with BaseSetup.configure = configure }
