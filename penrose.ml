open Graphics;;

(*Open a 800x600 window. Make sure to include a space
  at the beginning of the geometry specification!*)
let width = 1900;;
let height = 1000;;

open_graph (" "^(string_of_int width)^"x"^(string_of_int height)^"+0-0");;

let phi = (1.+.(sqrt 5.))/. 2.;;

let obtuse = true
let acute  = false

let random_color () =
  let r = (Random.int 51)*5 in
  begin
    r
  end;;

let set_random_color() =
  let r = random_color() in
  let g = random_color() in
  let b = random_color() in
  set_color (rgb r g b);; 

let distance (ax,ay) (bx,by) = sqrt((ax -. bx)**2. +. (ay -. by)**2.);;

let split_line (p1x,p1y) (p2x,p2y) =
  let dist=(distance (p1x,p1y) (p2x,p2y))/.((1.+.(sqrt 5.))/. 2.) in
  let k2 = dist /. phi in
  let k1 = dist -. k2 in
  ((p1x,p1y),((k1*.p2x+.k2*.p1x)/.(k1+.k2),(k1*.p2y+.k2*.p1y)/.(k1+.k2)),(p2x,p2y));;

let float_point_to_int (x,y)=
  (int_of_float x, int_of_float y)

let float_tri_to_int p: (int*int) array =
  [|float_point_to_int p.(0);float_point_to_int p.(1);float_point_to_int p.(2)|];;


Random.self_init;;

(*pre-cond:
    p: apex is always the first point*)
let rec divide gen div [|apex;s1;s2|] tyipe =
  (**)
  if gen <= 0 then 
    begin
      set_random_color();
      fill_poly (float_tri_to_int [|apex;s1;s2|]);
    end
  else if tyipe == obtuse
  then
    let (o_l, btw, a_h) = split_line s1 s2 in
    let a = [|a_h; apex; btw|] in
    let o = [|btw; apex; o_l|] in
    begin
      (*set_random_color();
        set_color blue;
        fill_poly (float_tri_to_int a);
        set_random_color();
        set_color red;
        fill_poly (float_tri_to_int o);*)
      divide (gen-1) div a acute;
      divide (gen-1) div o obtuse;
    end
  else
    let (_, btw, _) = split_line s1 apex in
    let (_, o_h, _) = split_line apex s2 in
    let a1 = [|s2; btw; s1|] in
    let a2 = [|s2; btw; o_h|] in
    let o = [|o_h; btw; apex|] in
    begin
      (*set_random_color();
        fill_poly (float_tri_to_int a1);
        set_random_color();
        fill_poly (float_tri_to_int a2);
        set_random_color();
        fill_poly (float_tri_to_int o);*)
      divide (gen-1) div a1 acute;
      divide (gen-1) div a2 acute;
      divide (gen-1) div o obtuse;
    end;;

let get_x (x,y) = x;;
let get_y (x,y) = y;;

let scale = 1 in
let center = (width/2,height/2) in
let s1 = (0.,0.) in
let s2 = (0.,float_of_int height) in
divide 10 0 [|
  ((sqrt phi**2. -. 0.25)*.(distance s1 s2),(distance s1 s2)/.2.);
  s1;
  s2
|] acute;;

ignore (Graphics.read_key ());;