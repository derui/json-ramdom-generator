open Core.Std

module S = Yojson.Safe

let make_random_string () =
  let len = (Random.int 20) + 1 in
  let chars = [|'a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z'|] in
  let chars_random = List.range 10 (len + 10) |> List.map ~f:(fun _ -> chars.(Random.int 26)) in
  List.map ~f:Char.to_string chars_random |> String.concat

let convert_variant ~name ~js =
  match name with
  | "random" -> begin
      match js with
      | None -> eprintf "'random' should have any value as type to specify\n"; exit 2
      | Some(`Int bound) -> `Int (Random.int bound) (* random of int returns between 0 and given value. *)
      | Some(`Float bound) -> `Float (Random.float bound) (* random of int returns between 0 and given value. *)
      | Some(`String _) -> `String (make_random_string ())
      | _ -> `Null
    end
  | _ -> eprintf "Must specify the name for converter is `random', not %s\n" name; exit 2

let generate_randomized_json json num =
  let rec replace json () =
    match json with
    | `List list -> `List (List.map ~f:(fun j -> replace j ()) list)
    | `Variant (name, js) -> convert_variant ~name ~js
    | `Assoc list -> `Assoc (List.map ~f:(fun (name, js) -> (name, replace js ())) list)
    | js -> js
  in
  List.range 0 num |>
    List.map ~f:(fun _ ->
                 let open Lazy in
                 from_fun (replace json) >>| (S.to_channel Out_channel.stdout)
                )

let main num path () =
  let filename = match Sys.is_file path with
    | `Yes -> path
    | _ -> eprintf "Template must be an file is exists\n"; exit 1
  in
  let js = S.from_file filename in
  generate_randomized_json js num |> List.map ~f:Lazy.force |> ignore

let spec = Command.Spec.(
    empty
    +> flag "-n" (optional_with_default 10 int) ~doc:"num Number of json to generate"
    +> flag "-t" (required string) ~doc:"template File path of template"
           )

let command = Command.basic
                ~summary:"Randomized Json Generator"
                ~readme:(fun () -> "More detaild information")
                spec
                main
let () =
  Time.now () |> Time.hash |> Random.init;
  Command.run ~version:"0.1" ~build_info:"derui" command
