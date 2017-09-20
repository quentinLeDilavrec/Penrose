#load "graphics.cma";; (* Load the graphics library *)
#load "unix.cma"
open Graphics;;
open Unix;;

(* Custom types *)
type point = float * float;;
type triangle = point * point * point;;
type triangle_type = Acute | Obtuse;;
type penrose_triangle = { points : triangle; ttype : triangle_type };;

(* Golden ratio *)
let phi = (1. +. (sqrt 5.)) /. 2.;;

(* Window parameters *)
let width = 1300
and height = 800;;

(* Return the length of \vec{AB} where A = (x1,y1) and B = (x2,y2) *)
let length ((x1, y1) : point) ((x2, y2) : point) =
  let x = x1 -. x2
  and y = y1 -. y2 in
    sqrt (x *. x +. y *. y);;

(* Given a triangle, return the triangle obtained by rounding each coordinate to the nearest integer.
   This function is used to render triangles using the Graphics module, which does not support floating
   point coordinates *)
let integer_triangle (t : triangle) =
  let apply_to_pair f (x, y) = (f x, f y)
  and apply_to_triple f (x, y, z) = (f x, f y, f z) in
    apply_to_triple (apply_to_pair int_of_float) t;;

(* Draw a triangle on screen *)
let draw_triangle (t : triangle) =
  let (a, b, c) = integer_triangle t in
    fill_poly [|a; b; c|];;

(* Set a random drawing color *)
let set_random_color () =
  let r = Random.int 255
  and g = Random.int 255
  and b = Random.int 255 in
    set_color (rgb r g b);;

(* Return the point on the line (AB) where the recursive division for the penrose tiling algorithm occurs, with A = (x1,y1) and B = (x2,y2) *)
let get_division_point ((x1,y1) : point) ((x2,y2) : point) =
  let len = length (x1,y1) (x2,y2) in
  let k1 = len /. phi in
  let k2 = len -. k1 in
  let sum = k1 +. k2 in
  let barycenter_x = (k2 *. x1 +. k1 *. x2) /. sum
  and barycenter_y = (k2 *. y1 +. k1 *. y2) /. sum in
    ((barycenter_x, barycenter_y) : point);;

(* Assumes that the triangles are given in counter-clockwise order, starting with their apex *)
let rec divide generation (t : penrose_triangle) =
  if generation = 0 then begin
    set_random_color ();
    (*let c = if t.ttype = Obtuse then rgb 255 255 0
      else rgb 0 0 255 in
      set_color c;*)
    (*sleepf 0.1;*)
    draw_triangle t.points
  end
  else
    let (a,b,c) = t.points in
    let division_point = 
      if t.ttype = Obtuse then get_division_point b c
      else get_division_point a b in
    let t1 =
      if t.ttype = Obtuse then {points = (b, division_point, a);
                                ttype = Acute}
      else {points = (c, division_point, b);
            ttype = Acute}
    and t2 = {points = (division_point, c, a);
              ttype = Obtuse} in
      if t.ttype = Acute then 
        (divide (generation - 1) t1; divide generation t2)
      else
        (divide (generation - 1) t1; divide (generation - 1) t2);;

(* Close any possible open window *)
close_graph();;

(* Create a new window *)
open_graph (" " ^ (string_of_int width) ^ "x" ^ (string_of_int height) ^ "-0+0");;

(* Initialize random number generator *)
Random.self_init;;

let half_float_height = 0.5 *. (float_of_int height) in
let start_triangle_apex = ((sqrt (phi *. phi -. 0.5 *. 0.5)) *. (float_of_int height), half_float_height) in
let (start_triangle : triangle) = (start_triangle_apex,(0.,float_of_int height),(0.,0.)) in
  divide 20 {points = start_triangle; ttype = Acute};;
