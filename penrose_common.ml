#load "graphics.cma";;
open Graphics;;


(* Golden ratio *)
let phi = (1.+.(sqrt 5.))/. 2.;;

(* Custom types *)
type point = float * float;;
type triangle = point * point * point;;
type triangle_type = Acute | Obtuse;;
type penrose_triangle = triangle * triangle_type;;

(* Calcul la distance entre 2 points *)
let distance ((ax,ay):point) ((bx,by):point) =
  let x = ax -. bx
  and y = ay -. by in
  sqrt (x *. x +. y *. y);;

(* Return the point on ab at |ab|/phi from a *)
let split_line ((ax,ay):point) ((bx,by):point) : point =
  let dist = distance (ax,ay) (bx,by) in
  let k2   = dist /. phi in
  let k1   = dist -. k2 in
  let sum  = k1 +. k2 in
  (*         x                 ,           y             *)
  ((k1*.bx +. k2*.ax)/. sum , (k1*.by +. k2*.ay)/. sum);;


(* Convert the given triangle to a triangle with integer coordinates *)
let integer_triangle (t : triangle) =
  let apply_to_pair f (x, y) = (f x, f y)
  and apply_to_triple f (x, y, z) = (f x, f y, f z) in
  apply_to_triple (apply_to_pair int_of_float) t;;

(* Draw a triangle on screen *)
let draw_triangle points =
  let (a,b,c)= integer_triangle points in
  fill_poly [|a;b;c|];;


(* Convert the given triangle to a triangle with integer coordinates *)
let integer_triangle (t : triangle) =
  let apply_to_pair f (x, y) = (f x, f y)
  and apply_to_triple f (x, y, z) = (f x, f y, f z) in
  apply_to_triple (apply_to_pair int_of_float) t;;

(* Draw a triangle on screen *)
let draw_triangle points =
  let (a,b,c)= integer_triangle points in
  fill_poly [|a;b;c|];;