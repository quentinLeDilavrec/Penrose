#load "graphics.cma";; 
open Graphics;;


(*------Parameters---------*)

(* Window parameters *)
let width = 1900;;
let height = 1000;;

let start_with_acute_triangle = true;;


(*---------------Tools----------------*)

(* Custom types *)
type point = float * float;;
type triangle = point * point * point;;
type triangle_type = Acute | Obtuse;;
type penrose_triangle = triangle * triangle_type;;
type penrose_triangle_old = {
  points:triangle;
  ttype:triangle_type
}


(* Golden ratio *)
let phi = (1.+.(sqrt 5.))/. 2.;;

let set_random_color() =
  let r = Random.int 255 
  and g = Random.int 255
  and b = Random.int 255
  in set_color (rgb r g b);; 

(* distance : point*point -> float
              a   ,   b   -> distance between a and b  *)
let distance ((ax,ay):point) ((bx,by):point) = 
  sqrt((ax -. bx)**2. +. (ay -. by)**2.);;

(* return the point on ab at |ab|/phi from a *)
let split_line ((ax,ay):point) ((bx,by):point) : point =
  let dist = distance (ax,ay) (bx,by) in
  let k2   = dist /. phi in
  let k1   = dist -. k2 in
  let sum  = k1 +. k2 in
  (*         x                 ,           y             *)
  ((k1*.bx +. k2*.ax)/. sum , (k1*.by +. k2*.ay)/. sum);;

let integer_triangle (t : triangle) =
  let apply_to_pair f (x, y) = (f x, f y)
  and apply_to_triple f (x, y, z) = (f x, f y, f z) in
  apply_to_triple (apply_to_pair int_of_float) t;;

let draw_triangle points =
  let (a,b,c)= integer_triangle points in
  fill_poly [|a;b;c|];;



(*------------Triangle divider algorithme for penrose paving------------
  pre-cond:
    p: - apex is always the first point
       - points aren't aligned
*)
let rec divide generation (t : penrose_triangle) =
  match t with
  |(triangle,_) when generation<=0 -> 
    set_random_color();
    draw_triangle triangle ;

  |((apex,s1,s2),Obtuse) ->
    let btw = split_line s2 s1 in
    let a = (s1, apex, btw) 
    and o = (btw, apex, s2) in
    begin
      divide (generation-1) (a,Acute);
      divide (generation-1) (o,Obtuse);
    end

  |((apex,s1,s2),_) ->
    let btw = split_line s1 apex
    and o_h = split_line apex s2 in
    let a1 = (s2, btw, s1) 
    and a2 = (s2, btw, o_h) 
    and o  = (o_h, btw, apex) in
    begin
      divide (generation-1) (a1,Acute);
      divide (generation-1) (a2,Acute);
      divide (generation-1) (o,Obtuse);
    end;;



(* Initialize random number generator *)
Random.self_init;;

(* Setup the first triangle to call it with divide *)
let s1 = (0.,0.)
and s2 = (0.,float_of_int height) in
let dist = (distance s1 s2)
and height_base_x_ratio = 
  if start_with_acute_triangle
  then sqrt (phi**2. -. 0.25)
  else sqrt (   1.   -. 0.25*.phi**2.) in
let apex = (height_base_x_ratio*.dist , dist/.2.) in
let starting_acute_triangle = (apex, s2, s1) in

divide 10 (starting_acute_triangle,
           if start_with_acute_triangle then Acute else Obtuse);;

(*Keep the graph open*)
ignore (Graphics.read_key ());;