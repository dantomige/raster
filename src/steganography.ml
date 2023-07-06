open Core

(* You need to change the implementation of this function so that it does
   something to the image instead of just leaving it untouched. *)
let transform image =
  let lower_order_image =
    Image.map image ~f:(fun (r, g, b) -> r % 4, g % 4, b % 4)
  in
  let brighten_image =
    Image.map lower_order_image ~f:(fun (r, g, b) ->
      r * Int.( ** ) 2 6, g * Int.( ** ) 2 6, b * Int.( ** ) 2 6)
  in
  brighten_image
;;

let command =
  Command.basic
    ~summary:"Finding secret message with steganography"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm"
             ^ "_steganography.ppm")]
;;
