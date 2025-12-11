open Core

let tmpdir = Filename_unix.temp_dir "c64nn" ""

let untar archive dst =
  let path = Filename_unix.realpath archive in
  let proc = Core_unix.create_process
    ~prog:"tar"
    ~args:["-axf"; path; "-C"; dst]
  in
  Core_unix.waitpid proc.pid
  |> Core_unix.Exit_or_signal.or_error
  |> Or_error.ok_exn

let command =
  Command.basic
    ~summary:"Compile an NNEF tarball into a C64 memory image"
    Command.Let_syntax.(
      let%map_open archive = anon ("TARBALL" %: string) in
      fun () -> untar archive tmpdir)

let () = Command_unix.run command
