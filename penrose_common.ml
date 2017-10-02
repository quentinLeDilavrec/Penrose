#use "graphics.cma";;
open Graphics;;


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
  barycenter (a,k1) (b,k2)

(* Draw a triangle on screen *)
let draw_triangle penrose_t =
  let f (Point(x,y)) = (int_of_float x,int_of_float y) in
  match penrose_t with
  | Acute t | Obtuse t ->
    fill_poly [|f t.apex;f t.s1;f t.s2|];;


class ['a] animation handler = object(self)

  val state = ((ref []):'a list ref)

  (* Handle the keybord inputs*)
  method private handler = handler
  (* Initialise tri_state *)
  method start first =
    state := [first];
    (loop_at_exit [Key_pressed] self#handler);

    (* Reinitialise tri_state *)
  method restart first =
    state := [first];

end;;