open Core

(* You need to change the implementation of this function so that it does
   something to the image instead of just leaving it untouched. *)

let sum_pixel_rgb_values (pixel : Pixel.t) : int =
  Pixel.red pixel + Pixel.green pixel + Pixel.blue pixel
;;

let get_squared_pixel_difference pixel1 pixel2 : Pixel.t =
  let r, g, b = Pixel.( - ) pixel1 pixel2 in
  r * r, g * g, b * b
;;

let mean_sqare_error (region1 : Image.t) (region2 : Image.t) : Float.t =
  let image_heights = Image.height region1 in
  let image_width = Image.width region2 in
  let total_squared_error =
    Image.foldi region1 ~init:0 ~f:(fun ~x ~y accu pixel ->
      accu
      + (get_squared_pixel_difference pixel (Image.get region2 ~x ~y)
         |> sum_pixel_rgb_values))
  in
  Float.of_int total_squared_error
  /. Float.of_int (image_heights * image_width)
;;

let create_region image top_right_x top_right_y width height =
  (* Image.slice image ~x_start:top_right_x ~x_end:top_right_x + width
     ~y_start:top_right_y ~y_end:top_right_y + height *)
  ()
;;

let get_random_region image width height =
  (* Get the first region, chosen arbitrary before grids are set. *)
  let top_right_x = Random.int (Image.width image - width) in
  let top_right_y = Random.int (Image.height image - height) in
  ()
;;

(* Divide the image into grids. *)

(* Iterate through the regions and select the region with the smallest
   mse. *)

(* Swap the pixels of the two regions. *)

let swap_regions image region1 region2 = ()
let transform image : Image.t = image

let command =
  Command.basic
    ~summary:"Convert an image to grayscale"
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
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_gray.ppm")]
;;
