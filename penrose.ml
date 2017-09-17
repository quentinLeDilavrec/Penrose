open Graphics;;

type point = float * float;;
type triangle = point * point * point;;
type triangle_type = Acute | Obtuse;;
type penrose_triangle = {
  points:triangle;
  triangle_type:triangle_type
}
let f ({points=a;triangle_type=b}:penrose_triangle)=(a,b);;

let width = 1900;;
let height = 1000;;
let instant = false;;(*true if you just want to render a cetain stage else false for the dynamic rendering*)

(*Open a 800x600 window. Make sure to include a space
  at the beginning of the geometry specification!*)
open_graph (" "^(string_of_int width)^"x"^(string_of_int height)^"+0-0");;


let phi = (1.+.(sqrt 5.))/. 2.;;

let obtuse = true
let acute  = false

let set_random_color() =
  let r = Random.int 255 in
  let g = Random.int 255 in
  let b = Random.int 255 in
  set_color (rgb r g b);; 

let distance (ax,ay) (bx,by) = sqrt((ax -. bx)**2. +. (ay -. by)**2.);;

let split_line ((p1x,p1y):point) ((p2x,p2y):point) =
  let dist=(distance (p1x,p1y) (p2x,p2y)) in
  let k2 = dist /. phi in
  let k1 = dist -. k2 in
  let k1_plus_k2=k1+.k2 in (*factor or not?*)
  (((p1x,p1y):point),
   (((k1*.p2x+.k2*.p1x)/.k1_plus_k2,
     (k1*.p2y+.k2*.p1y)/.k1_plus_k2):point),
   ((p2x,p2y):point));;

let apply_to_double f (x,y) =
  (f x, f y);;

let apply_to_triple f (a,b,c) =
  (f a, f b, f c);;

let float_tri_to_int p =
  apply_to_triple (apply_to_double int_of_float) p;;

let draw_triangle points =
  let (a,b,c)= float_tri_to_int points in
  fill_poly [|a;b;c|];;

(*
  pre-cond:
    p: apex is always the first point
        point aren't aligned*)
let rec divide generation division ((apex,s1,s2) : triangle) tri_type =
  (**)
  if generation <= 0 then 
    begin
      set_random_color();
      draw_triangle (apex,s1,s2);
    end
  else if tri_type == Obtuse then
    let (s1,s2,division) = 
      if division then (s1,s2,division) else (s2,s1,not division) in
    let (o_l, btw, a_h) = split_line s1 s2 in
    let a = (a_h, apex, btw) in
    let o = (btw, apex, o_l) in
    begin
      divide (generation-1) division a Acute;
      divide (generation-1) division o Obtuse;
    end
  else
    let (s1,s2,division)= if division then (s1,s2,division) else (s2,s1,not division) in
    let (_, btw, _) = split_line s1 apex in
    let (_, o_h, _) = split_line apex s2 in
    let a1 = (s2, btw, s1) in
    let a2 = (s2, btw, o_h) in
    let o  = (o_h, btw, apex) in
    begin
      divide (generation-1) division a1 Acute;
      divide (generation-1) division a2 Acute;
      divide (generation-1) division o Obtuse;
    end;;

let utf8_char_to_comprensive_string c =
  match int_of_char c with
    n when n<10  -> "\\00" ^ string_of_int n;
  | n when n<100 -> "\\0"  ^ string_of_int n;
  | n            -> "\\"   ^ string_of_int n;;

exception Type_error;;

let divide_once ({points=(apex,s1,s2);triangle_type=tri_type}:penrose_triangle) :penrose_triangle list=
  match tri_type with
    Obtuse ->
    let (o_l, btw, a_h) = split_line s1 s2 in
    let a:triangle = (a_h, apex, btw) in
    let o:triangle = (btw, apex, o_l) in
    begin
      set_random_color();
      draw_triangle a;
      set_random_color();
      draw_triangle o;
      {points=a;triangle_type=Acute}::{points=o;triangle_type=Obtuse}::[]
    end;
  | Acute -> 
    let (_, btw, _) = split_line s1 apex in
    let (_, o_h, _) = split_line apex s2 in
    let a1:triangle = (s2, btw, s1) in
    let a2:triangle = (s2, btw, o_h) in
    let o:triangle  = (o_h, btw, apex) in
    begin
      set_random_color();
      draw_triangle a1;
      set_random_color();
      draw_triangle a2;
      set_random_color();
      draw_triangle o;
      {points=a1;triangle_type=Acute}::
      {points=a2;triangle_type=Acute}::
      {points= o;triangle_type=Obtuse}::[]
    end;;



let animation = object(self)

  val mutable tri_state : penrose_triangle list = [];

  method private handler x =
    match x.key with
      '\027' -> raise Exit;
    |'\032' -> let a = List.flatten (List.map( fun x -> divide_once x) tri_state) in tri_state <- a;();
    | a -> draw_string ((utf8_char_to_comprensive_string a)^" ");
      ()

  method start first_penrose_triangle   =
    begin
      tri_state <- [first_penrose_triangle];
      (loop_at_exit [Key_pressed] self#handler);
      ();
    end;

  method restart first_penrose_triangle =
    clear_graph(); 
    tri_state <- [first_penrose_triangle];
    ();
end;;


Random.self_init;;

let get_x (x,y) = x;;
let get_y (x,y) = y;;

let scale = 1 in
let center = (width/2,height/2) in
let s1 = (0.,0.) in
let s2 = (0.,float_of_int height) in
let starting_acute_triangle = (
  ((sqrt phi**2. -. 0.25)*.(distance s1 s2),(distance s1 s2)/.2.),
  s1,
  s2) in
if instant then
  divide 4 false starting_acute_triangle Acute 
else animation#start ({points=starting_acute_triangle;triangle_type=Acute}:penrose_triangle);;
(*ignore (Graphics.read_key ());;*)