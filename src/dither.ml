open Core

(* This should look familiar by now! *)
let boundary_check (t : Image.t) ~x ~y =
  (x >= 0 && x < Image.width t) && y >= 0 && y < Image.height t
;;

let set_pixel_and_find_error
  (x : int)
  (y : int)
  (image : Image.t)
  current_pixel
  white_pixel
  black_pixel
  pixel_threshold
  =
  if Pixel.red current_pixel > pixel_threshold
  then (
    Image.set image ~x ~y white_pixel;
    Pixel.red current_pixel - Image.max_val image)
  else (
    Image.set image ~x ~y black_pixel;
    Pixel.red current_pixel)
;;

let iterate_adjacent_pixels image x y (error : int) =
  let adjacent_dir_and_error_portion_list =
    [ (1, 0), Pixel.of_int (Int.of_float (Float.of_int error *. 7. /. 16.))
    ; (-1, 1), Pixel.of_int (Int.of_float (Float.of_int error *. 3. /. 16.))
    ; (0, 1), Pixel.of_int (Int.of_float (Float.of_int error *. 5. /. 16.))
    ; (1, 1), Pixel.of_int (Int.of_float (Float.of_int error *. 1. /. 16.))
    ]
  in
  List.iter
    adjacent_dir_and_error_portion_list
    ~f:(fun ((x_delta, y_delta), error_pixel) ->
    if boundary_check image ~x:(x + x_delta) ~y:(y + y_delta)
    then (
      let new_x, new_y = x + x_delta, y + y_delta in
      let new_error_pixel =
        Pixel.( + ) error_pixel (Image.get image ~x:new_x ~y:new_y)
      in
      Image.set image ~x:(x + x_delta) ~y:(y + y_delta) new_error_pixel)
    else ())
;;

(*Takes an image and position and changes the pixel at position to black or
  white pixel. Then adds the error to adjacent pixels*)
let change_adjacent_pixels
  (x : int)
  (y : int)
  (image : Image.t)
  (current_pixel : Pixel.t)
  (white_pixel : Pixel.t)
  (black_pixel : Pixel.t)
  (pixel_threshold : int)
  : Image.t
  =
  let error =
    set_pixel_and_find_error
      x
      y
      image
      current_pixel
      white_pixel
      black_pixel
      pixel_threshold
  in
  iterate_adjacent_pixels image x y error;
  image
;;

let transform image =
  let grey_image = Grayscale.transform image in
  let white_pixel = Pixel.of_int (Image.max_val image) in
  let pixel_threshold = Image.max_val image / 2 in
  let black_pixel = Pixel.zero in
  Image.foldi grey_image ~init:grey_image ~f:(fun ~x ~y image pixel ->
    change_adjacent_pixels
      x
      y
      image
      pixel
      white_pixel
      black_pixel
      pixel_threshold)
;;

let command =
  Command.basic
    ~summary:"Dither an image"
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
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;
