#load "graphics.cma";;
open Graphics;;


(*------Types, Constants and Parameters---------*)

(* Custom types *)
type point = float * float;;
type triangle = point * point * point;;
type triangle_type = Acute | Obtuse;;
type penrose_triangle = triangle * triangle_type;;

(* Golden ratio *)
let phi = (1.+.(sqrt 5.))/. 2.;;

(* Generation parameters *)
let start_with_acute_triangle = true;;
let iterations = 6;;

(* Window parameters *)
let height = 800;;
let width = (* Make sure the triangle fits in the window space *)
  if start_with_acute_triangle then int_of_float ((float_of_int height) *. phi)
  else height;;

(*---------------Utilities----------------*)

(* Set the frawing color to a random one *)
let set_random_color() =
  let r = Random.int 255
  and g = Random.int 255
  and b = Random.int 255
  in set_color (rgb r g b);;

(* distance : point*point -> float
              a   ,   b   -> distance between a and b  *)
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


(*------------Recursive triangle division algorithm------------
  Precondition:
    t: - apex is always the first point
       - points aren't aligned
*)
let rec divide generation (t : penrose_triangle) =
  match t with
  |(triangle,_) when generation=0 ->
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


(*----------------Main program----------------*)

(* Close any possible open window *)
close_graph();;

(* Create a new window *)
open_graph (" "^(string_of_int width)^"x"^(string_of_int height)^"+0-0");;

(* Initialize random number generator *)
Random.self_init;;

(* Setup the starting triangle for the first iteration *)
let s1 = (0.,0.)
and s2 = (0.,float_of_int height) in
let dist = (distance s1 s2) in
let height =
  if start_with_acute_triangle
  then (sqrt (phi**2. -. 0.25)) *. dist
  else (sqrt (   1.   -. 0.25*.phi**2.)) *. (dist /. phi) in
let apex = (height, dist/.2.) in
let starting_triangle = (apex, s2, s1) in

divide iterations (starting_triangle,
           if start_with_acute_triangle then Acute else Obtuse);;

(* Keep the graph open until a key is pressed *)
ignore (Graphics.read_key ());;