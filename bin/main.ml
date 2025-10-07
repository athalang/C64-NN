open Core

let tmpdir = Filename_unix.temp_dir "c64nn" ""

let untar_unzip filename =
  let realpath = Filename_unix.realpath filename in
  match Tar_unix.extract ~src:realpath tmpdir with
  | Ok () -> ()
  | Error `Fatal e ->
    Tar.pp_error Format.err_formatter e;
    Out_channel.newline stderr;
    prerr_endline ("from: " ^ realpath)
  | Error `Unix (err, _, _) ->
    prerr_endline ("Unix error: " ^ Core_unix.Error.message err);
    prerr_endline ("from: " ^ realpath)
  | Error `Unexpected_end_of_file ->
    prerr_endline ("Unexpected EOF error: " ^ realpath)
  | Error `Msg s ->
    prerr_endline ("Error: " ^ s);
    prerr_endline ("from: " ^ realpath)
  | Error `Exn _ ->
    prerr_endline ("Exception when extracting NNEF from: " ^ realpath)

let command =
  Command.basic
    ~summary:"Compile an NNEF gzipped tarball into a C64 memory image"
    (Command.Param.(
      map
        (anon ("filename" %: string))
        ~f:(fun filename () ->
          untar_unzip filename)))

let () = Command_unix.run command