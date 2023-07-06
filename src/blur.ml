open Core

(* You need to modify this function to blur the input image based on the
   provided radius instead of ignoring it. *)
let transform image ~radius =
  (* Helper funtion to get the image borders for a given pixel *)
  let get_image_borders (x : int) (y : int) (radius : int) =
    ( max (x - radius) 0
    , min (x + radius) (Image.width image)
    , max (y - radius) 0
    , min (y + radius) (Image.height image) )
  in
  (* Helper function to get the image mean from image borders *)
  let image_mean_from_borders ~x_start ~x_end ~y_start ~y_end =
    let kernel = Image.slice image ~x_start ~x_end ~y_start ~y_end in
    Image.mean_pixel kernel
  in
  Image.mapi image ~f:(fun ~x ~y _ ->
    let x_start, x_end, y_start, y_end = get_image_borders x y radius in
    image_mean_from_borders ~x_start ~x_end ~y_start ~y_end)
;;

let command =
  Command.basic
    ~summary:"Blur an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and radius =
        flag
          "radius"
          (required Command.Param.int)
          ~doc:"N the radius to use when blurring (higher = more blurred)"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~radius in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_blur.ppm")]
;;
