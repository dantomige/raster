open! Core

let command =
  Command.group
    ~summary:"A tool to perform various image manipulations"
    [ "grayscale", Grayscale.command
    ; "bluescreen", Blue_screen.command
    ; "blur", Blur.command
    ; "dither", Dither.command
    ; "steganography", Steganography.command
    ; "improved-bluescreen", Improved_blue_screen.command
    ; "mosaic", Mosaic.command
    ; "edge-detection", Edge_detection.command
    ]
;;
