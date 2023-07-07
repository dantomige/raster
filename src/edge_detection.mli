val horizontal_gradient_kernel : ((int * int) * int) list
val vertical_gradient_kernel : ((int * int) * int) list
val boundary_check : Image.t -> x:int -> y:int -> bool
val convolution : Image.t -> int -> int -> ((int * int) * int) list -> int
val transform : Image.t -> float -> Image.t
val command : Command.t
