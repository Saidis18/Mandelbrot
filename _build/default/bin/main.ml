open Mandelbrot.Lib
open Graphics

let f (x_c, y_c) (x, y) = (x *. x -. y *. y +. x_c, 2. *. x *. y +. y_c)


let r = mandelbrot (-0.31, 0.06) (0.64, 1.15) 680 390 90 f 1000000.

let () = open_graph " 420x720"; draw_image (make_image r) 10 10; ignore (input_line stdin)



(*
let r = mandelbrot (-0.31, 0.06) (0.64, 1.15) 1350 750 90 f 1000000.

let () = open_graph " 1350x750"; draw_image (make_image @@ transpose r) 10 10; ignore (input_line stdin)
*)


