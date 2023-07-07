open Core

(* Mosaic implementation*)

let sum_pixel_rgb_values (pixel : Pixel.t) : float =
  Float.of_int (Pixel.red pixel + Pixel.green pixel + Pixel.blue pixel)
;;

let get_squared_pixel_difference pixel1 pixel2 : Pixel.t =
  let r, g, b = Pixel.( - ) pixel1 pixel2 in
  r * r, g * g, b * b
;;

let create_region image ~top_left_x ~top_left_y ~width ~height =
  Image.slice
    image
    ~x_start:top_left_x
    ~x_end:(top_left_x + width)
    ~y_start:top_left_y
    ~y_end:(top_left_y + height)
;;

let calculate_mean_sqare_error
  image
  reference_region
  (top_left_region2 : int * int)
  width
  height
  : float
  =
  let region1 = reference_region in
  let region2 =
    create_region
      image
      ~top_left_x:(fst top_left_region2)
      ~top_left_y:(snd top_left_region2)
      ~width
      ~height
  in
  let image_height = Image.height image in
  let image_width = Image.width image in
  let total_squared_error =
    Image.foldi region1 ~init:0. ~f:(fun ~x ~y accu pixel ->
      accu
      +. (get_squared_pixel_difference pixel (Image.get region2 ~x ~y)
          |> sum_pixel_rgb_values))
  in
  total_squared_error /. Float.of_int (image_height * image_width)
;;

let get_random_region image width height =
  (* Get the first region, chosen arbitrary before grids are set. *)
  let top_left_x = Random.int (Image.width image - width) in
  let top_left_y = Random.int (Image.height image - height) in
  ( top_left_x
  , top_left_y
  , create_region image ~top_left_x ~top_left_y ~width ~height )
;;

let get_grid_positions image width height =
  List.init
    (Image.width image / width)
    ~f:(fun x_pos ->
      List.init
        (Image.height image / height)
        ~f:(fun y_pos -> x_pos * width, y_pos * height))
  |> List.concat
;;

let find_min_grid_region image width height reference_region =
  let min_mse_and_region = Float.infinity, (0, 0) in
  (* Divide the image into grids. *)
  let grid_top_left_pos = get_grid_positions image width height in
  (* Iterate through the regions and select the region with the smallest
     mse. *)
  let min_mse_and_region =
    List.fold
      grid_top_left_pos
      ~init:min_mse_and_region
      ~f:(fun (min_region_mse, min_region) top_left_other_region ->
      let current_mse =
        calculate_mean_sqare_error
          image
          reference_region
          top_left_other_region
          width
          height
      in
      if Float.( <. ) current_mse min_region_mse
      then current_mse, top_left_other_region
      else min_region_mse, min_region)
  in
  snd min_mse_and_region
;;

(* Swap the pixels of the two regions. *)
let swap_regions
  original_image
  reference_region
  top_left_region1
  top_left_region2
  =
  Image.foldi
    reference_region
    ~init:original_image
    ~f:(fun ~x ~y image pixel ->
    let curr_pixel_x, curr_pixel_y =
      fst top_left_region1 + x, snd top_left_region1 + y
    in
    let other_pixel_x, other_pixel_y =
      fst top_left_region2 + x, snd top_left_region2 + y
    in
    let other_pixel =
      Image.get original_image ~x:other_pixel_x ~y:other_pixel_y
    in
    Image.set image ~x:curr_pixel_x ~y:curr_pixel_y other_pixel;
    Image.set image ~x:other_pixel_x ~y:other_pixel_y pixel;
    image)
;;

let perform_region_swap image width height =
  let top_left_x, top_left_y, reference_region =
    get_random_region image width height
  in
  let other_top_left_x, other_top_right_x =
    find_min_grid_region image width height reference_region
  in
  swap_regions
    image
    reference_region
    (top_left_x, top_left_y)
    (other_top_left_x, other_top_right_x)
;;

let transform image ~width ~height ~moves : Image.t =
  let rec perform_n_region_swaps image width height n =
    if n = 0
    then image
    else
      perform_n_region_swaps
        (perform_region_swap image width height)
        width
        height
        (n - 1)
  in
  perform_n_region_swaps image width height moves
;;

let command =
  Command.basic
    ~summary:"Creating a mosaic"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image =
          Image.load_ppm ~filename
          |> fun image -> transform image ~width:10 ~height:10 ~moves:1000
        in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_mosaic.ppm")]
;;
