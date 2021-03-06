#load "graphics.cma";;
open Graphics;;


(*---------------Utilities----------------*)

(* Set the frawing color to a random one *)
let set_random_color() =
  let r = Random.int 255
  and g = Random.int 255
  and b = Random.int 255
  in set_color (rgb r g b);;



(*------Types, Constants and Parameters---------*)
(* Golden ratio *)
let phi = (1.+.(sqrt 5.))/. 2.;;

(* Custom types *)
type point = Point of float * float;;
type triangle_iso = {apex:point;s1:point;s2:point};;
type penrose_triangle = Acute of triangle_iso | Obtuse of triangle_iso;;

(* Calcul la distance entre 2 points *)
let distance (Point(ax,ay)) (Point(bx,by)) =
  let x = ax -. bx
  and y = ay -. by in
  sqrt (x *. x +. y *. y);;

(* Return the barycenter of ab weighted by k1 and k2 *)
let barycenter (Point(ax,ay),k1) (Point(bx,by),k2) =
  let sum  = k1 +. k2 in
  (*         x              ,           y             *)
  Point((k1*.ax +. k2*.bx)/. sum , (k1*.ay +. k2*.by)/. sum);;

(* Return the point on ab at |ab|/phi from a *)
let split_line a b =
  let dist = distance a b in
  let k1   = dist /. phi in
  let k2   = dist -. k1 in
  barycenter (a,k1) (b,k2);;

(* Draw a triangle on screen *)
let draw_triangle t =
  let f (Point(x,y)) = (int_of_float x,int_of_float y) in
  fill_poly [|f t.apex;f t.s1;f t.s2|];;

let is_obtuse t =
  match t with
  | Obtuse _ -> true
  | _ -> false;;

let get_triangle penrose_t =
  match penrose_t with
  | Obtuse t| Acute t -> t;;

(*****************************************************)
(* Generation parameters *)
let start_with_acute_triangle = true;;
let iterations = 6;;

(* Window parameters *)
let height = 800;;
let width = (* Make sure the triangle fits in the window space *)
  if start_with_acute_triangle then int_of_float ((float_of_int height) *. phi)
  else height;;



(*------------Recursive triangle division algorithm------------
  Precondition:
    t: - apex is always the first point
       - points aren't aligned
*)
let rec divide generation (penrose_t : penrose_triangle) =
  let t = get_triangle penrose_t in
  if generation = 0
  then begin
    set_random_color();
    draw_triangle t;
  end
  else if is_obtuse penrose_t then
    let btw = split_line t.s2 t.s1 in
    let a = {apex=t.s1; s1=t.apex; s2=btw}
    and o = {apex=btw; s1=t.apex; s2=t.s2} in
    begin
      divide (generation-1) (Acute a);
      divide (generation-1) (Obtuse o);
    end
  else
    let btw = split_line t.s1 t.apex
    and o_h = split_line t.apex t.s2 in
    let a1 = {apex=t.s2; s1=btw; s2=t.s1}
    and a2 = {apex=t.s2; s1=btw; s2=o_h}
    and o  = {apex=o_h; s1=btw; s2=t.apex} in
    begin
      divide (generation-1) (Acute a1);
      divide (generation-1) (Acute a2);
      divide (generation-1) (Obtuse o);
    end;;


(*----------------Main program----------------*)

(* Close any possible open window *)
close_graph();;

(* Create a new window *)
open_graph (" "^(string_of_int width)^"x"^(string_of_int height)^"+0-0");;

(* Initialize random number generator *)
Random.self_init;;

(* Setup the starting triangle for the first iteration *)
let s1 = Point (0.,0.)
and s2 = Point(0.,float_of_int height) in
let dist = distance s1 s2 in
let height =
  if start_with_acute_triangle
  then (sqrt (phi**2. -. 0.25)) *. dist
  else (sqrt (   1.   -. 0.25*.phi**2.)) *. (dist /. phi) in
let apex = Point (height, dist/.2.) in
let starting_triangle = {apex=apex; s1=s1; s2=s2} in

divide iterations (if start_with_acute_triangle 
                   then Acute starting_triangle 
                   else Obtuse starting_triangle);;

(* Keep the graph open until a key is pressed *)
ignore (Graphics.read_key ());;