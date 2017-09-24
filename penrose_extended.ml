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

(* Window parameters *)
let height = 600;;
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
  sqrt((ax -. bx)**2. +. (ay -. by)**2.);;

(* Return the point on ab at |ab|/phi from a *)
let split_line ((p1x,p1y):point) ((p2x,p2y):point) : point =
  let dist = distance (p1x,p1y) (p2x,p2y) in
  let k2   = dist /. phi in
  let k1   = dist -. k2 in
  let sum  = k1 +. k2 in
  (*         x                 ,           y             *)
  ((k1*.p2x +. k2*.p1x)/. sum , (k1*.p2y +. k2*.p1y)/. sum);;

(* Convert the given triangle to a triangle with integer coordinates *)
let integer_triangle (t : triangle) =
  let apply_to_pair f (x, y) = (f x, f y)
  and apply_to_triple f (x, y, z) = (f x, f y, f z) in
  apply_to_triple (apply_to_pair int_of_float) t;;

(* Draw a triangle on screen *)
let draw_triangle points =
  let (a,b,c)= integer_triangle points in
  fill_poly [|a;b;c|];;

(*------------Triangle division algorithm------------------------------------------------
  Apply one step of the recursive subdivision process which generates a Penrose tiling.
  Precondition:
    t: - apex is always the first point
       - points aren't aligned
*)
let divide_once (t : penrose_triangle) =
  match t with
  |((apex,s1,s2),Obtuse) ->
    let btw = split_line s2 s1 in
    let a = (s1, apex, btw) 
    and o = (btw, apex, s2) in
    begin
      set_random_color();
      draw_triangle a;
      set_random_color();
      draw_triangle o;
      [(a,Acute);(o,Obtuse)]
    end;
  |((apex,s1,s2),_) ->
    let btw = split_line s1 apex
    and o_h = split_line apex s2 in
    let a1 = (s2, btw, s1) 
    and a2 = (s2, btw, o_h) 
    and o  = (o_h, btw, apex) in
    begin
      set_random_color();
      draw_triangle a1;
      set_random_color();
      draw_triangle a2;
      set_random_color();
      draw_triangle o;
      [(a1,Acute);
       (a2,Acute);
       (o,Obtuse)]
    end;;


(*------------------Animation management-----------------------
  Manages the animation, the inputs and the only mutable state *)
let animation = object(self)

  val mutable tri_state : penrose_triangle list = []

  (* Handle the keybord inputs*)
  method private handler x =
    match x.key with
    | '\027' -> raise Exit;
    | ' '    -> tri_state <- tri_state
                             |> List.map divide_once
                             |> List.concat;
    | other  -> draw_string ((Char.escaped other)^" ")

  (* Initialise tri_state *)
  method start first_penrose_triangle =
    set_random_color();
    let (triangle,_) = first_penrose_triangle in 
    draw_triangle triangle;
    tri_state <- [first_penrose_triangle];
    (loop_at_exit [Key_pressed] self#handler);

  (* Reinitialise tri_state *)
  method restart first_penrose_triangle =
    set_random_color();
    let (triangle,_) = first_penrose_triangle in 
    draw_triangle triangle;
    tri_state <- [first_penrose_triangle];

end;;


(*-------------Main program--------------------------------*)     

(* Close any possible open window *)
close_graph();;

(* Create a new window *)
open_graph (" "^(string_of_int width)^"x"^(string_of_int height)^"+0-0");;

(* Initialize random number generator *)
Random.self_init;;

(* Setup the first triangle before starting*)
let s1 = (0.,0.)
and s2 = (0.,float_of_int height) in
let dist = (distance s1 s2) in
let height = 
  if start_with_acute_triangle
  then sqrt ((phi**2. -. 0.25) *.dist)
  else sqrt ((   1.   -. 0.25*.phi**2.)) *.dist/.phi in
let apex = (height , dist/.2.) in
let starting_acute_triangle = (apex, s2, s1) in
let starting_triangle_type = if start_with_acute_triangle then Acute else Obtuse in

animation#start (starting_acute_triangle, starting_triangle_type);;

