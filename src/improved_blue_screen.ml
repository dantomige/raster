open Core

(* You need to change the implementation of this function so that it replaces
   the "blue" pixels of the foreground image with pixels from the
   corresponding position in the background image instead of just ignoring
   the background image and returning the foreground image. *)
let transform ~foreground ~background =
  let is_blueish pixel =
    let r, g, b = pixel in
    Float.( > ) (Float.of_int b) (Float.of_int (r + g) *. 1.01)
  in
  let convert_blueish_pixels ~x ~y (pixel : Pixel.t) =
    if is_blueish pixel then Image.get background ~x ~y else pixel
  in
  Image.mapi foreground ~f:convert_blueish_pixels
;;

let command =
  Command.basic
    ~summary:
      "Replace the 'blue' pixels of an image with those from another image \
       (improved-bluescreening)"
    [%map_open.Command
      let foreground_file =
        flag
          "foreground"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the foreground PPM image file"
      and background_file =
        flag
          "background"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the background PPM image file"
      in
      fun () ->
        let foreground = Image.load_ppm ~filename:foreground_file in
        let background = Image.load_ppm ~filename:background_file in
        let image' = transform ~foreground ~background in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn foreground_file ~suffix:".ppm"
             ^ "_vfx-improved.ppm")]
;;
