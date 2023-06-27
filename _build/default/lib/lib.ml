open Graphics

let iof = int_of_float
let foi = float_of_int

let test_convergence f c eps (n: int) =
  let rec aux z = function
    |k when k >= n -> (-1: int)
    |k when (fst z) *. (fst z) +. (snd z) *. (snd z) >= eps -> (k: int)
    |k -> aux (f c z) (k+1)
in aux (0., 0.) 0

let degrad k n =
  let x = (foi k) /.(foi n) in
  -. (x -. 1.) *. (x -. 1.) +. 1.

let couleur (n: int) = function
  | (-1: int) -> rgb 10 10 70
  | (k: int) -> rgb 0 (iof (255. *. degrad k n) ) (iof (255. *.(1. -. degrad k n)) )

let coord (x_min, x_max) (y_min, y_max) (i: int) (j: int) (n: int) (m: int) =
  let x = x_min +. ((x_max -. x_min) /. foi m) *. foi j in
  let y = y_min +. ((y_max -. y_min) /. foi n) *. foi (n - i) in
  (x, y)

  let mandelbrot (x_min, x_max) (y_min, y_max) (n: int) (m: int) (k: int) f eps =
    let r = Array.make_matrix n m black in
    for i = 0 to n - 1 do
      for j = 0 to m - 1 do
        let c = coord (x_min, x_max) (y_min, y_max) i j n m in
        r.(i).(j) <- couleur k (test_convergence f c eps k)
      done;
    done;
    r

  let transpose a =
    let n = Array.length a in
    let p = Array.length a.(0) in
    let r = Array.make_matrix p n a.(0).(0) in
    for i = 0 to n - 1 do
      for j = 0 to p - 1 do
        r.(j).(i) <- a.(i).(j)
      done;
    done;
    r