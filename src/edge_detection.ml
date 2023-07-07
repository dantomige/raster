open Core

(* This should look familiar by now! *)
let horizontal_gradient_kernel =
  [ (-1, -1), -1
  ; (1, -1), 1
  ; (-1, 0), -2
  ; (1, 0), 2
  ; (-1, 1), -1
  ; (1, 1), 1
  ]
;;

let vertical_gradient_kernel =
  [ (-1, -1), -1
  ; (0, -1), -2
  ; (1, -1), -1
  ; (-1, 1), 1
  ; (0, 1), 2
  ; (1, 1), 1
  ]
;;

let boundary_check (t : Image.t) ~x ~y =
  (x >= 0 && x < Image.width t) && y >= 0 && y < Image.height t
;;

let convolution image x y kernel =
  List.fold kernel ~init:0 ~f:(fun acc ((x_delta, y_delta), kernel_value) ->
    if boundary_check image ~x:(x + x_delta) ~y:(y + y_delta)
    then (
      let new_x, new_y = x + x_delta, y + y_delta in
      acc + (kernel_value * Pixel.red (Image.get image ~x:new_x ~y:new_y)))
    else acc)
;;

(*Applies convolution to each pixel*)

let transform image threshold =
  let transformed_image =
    Grayscale.transform image |> fun image -> Blur.transform image ~radius:2
  in
  let threshold_value = Float.of_int (Image.max_val image) *. threshold in
  Image.mapi transformed_image ~f:(fun ~x ~y _ ->
    let gx =
      Float.of_int
        (convolution transformed_image x y horizontal_gradient_kernel)
    in
    let gy =
      Float.of_int
        (convolution transformed_image x y vertical_gradient_kernel)
    in
    let g_inner = Float.( ** ) gx 2. +. Float.( ** ) gy 2. in
    let g = sqrt g_inner in
    if Float.( >. ) g threshold_value
    then Pixel.of_int (Image.max_val transformed_image)
    else Pixel.zero)
;;

let command =
  Command.basic
    ~summary:"Find the edges of an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image =
          Image.load_ppm ~filename |> fun image -> transform image 0.4
        in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm"
             ^ "_edge-detection.ppm")]
;;
