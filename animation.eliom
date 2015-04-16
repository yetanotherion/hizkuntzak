(**************************************************************************)
(*  Copyright 2014, Ion Alberdi <nolaridebi at gmail.com>                 *)
(*                                                                        *)
(*  Licensed under the Apache License, Version 2.0 (the "License");       *)
(*  you may not use this file except in compliance with the License.      *)
(*  You may obtain a copy of the License at                               *)
(*                                                                        *)
(*      http://www.apache.org/licenses/LICENSE-2.0                        *)
(*                                                                        *)
(*  Unless required by applicable law or agreed to in writing, software   *)
(*  distributed under the License is distributed on an "AS IS" BASIS,     *)
(*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or       *)
(*  implied.  See the License for the specific language governing         *)
(*  permissions and limitations under the License.                        *)
(**************************************************************************)
{client{

type point = {
  x: float;
  y: float;
}

let inv_y p = {y=0.0 -. p.y;
               x=p.x}

let range ?step:(s=1) start_idx end_idx =
  (* range 0 3 == [0; 1; 2] *)
  let rec _range cidx eidx accum =
    if cidx + s >= eidx then List.rev (cidx :: accum)
    else _range (cidx + s) eidx (cidx :: accum)
  in
  _range start_idx end_idx []

let enumerate l =
  let _, r = List.fold_left (fun (curr_idx, res) elt -> (curr_idx + 1, (curr_idx, elt) :: res)) (0, []) l in
  List.rev r

let xrange src dst ns =
  (* ns must be > 1.
     returns x-axis coordinates
     that go from src to dest
     in ns steps, by ensuring that
     - at step 0, we start at src.x
     - at step ns - 1, we are at dst.x
  *)
  let () =
    if ns < 2 then raise (Failure "xrange cannot work with ns<2")
  in
  let start_x = src.x in
  let x_step = (dst.x -. src.x) /. (float_of_int ns) in
  List.map (fun (i, x) ->
    if i = (ns - 1) then dst.x
    else start_x +. (float_of_int i) *. x_step)
    (enumerate (range 0 ns))

module Line = struct
  type t = {
    slope: float;
    origin: float;
  }

  let create src dest =
    let () =
      let error = Printf.sprintf "src and dest must have different x-labels got %f %f %f %f" src.x dest.x src.y dest.y in
      if dest.x == src.x then failwith(error)
    in
    let slope = (dest.y -. src.y) /. (dest.x -. src.x) in
    { slope = slope;
      origin = src.y -. slope *. src.x}

  let y_axis t x_axis =
    t.slope *. x_axis +. t.origin
end

let abs x = if x < 0.0 then 0.0 -. x else x

module VerticalJump = struct
  let g = 9.91
  (* y = - g * t^2 / 2 + v0 * t + yO *)

  type t = {
    v0: float;
    y0: float;
  }

  let create src dest =
    let () =
      if src.x != dest.x then failwith("src and dest must have the same x-label")
    in
    (* max in b - g * t = 0 *)
    (* i.e. b / g = t *)
    (* we want that the max is in dest.y *)
    (* dest.y = src.y + b * b / g - g / 2 * (b / g) * (b / g) *)
    (* dest.y = src.y + b^2 / g - (b^2 / (g * 2) *)
    (* dest.y = src.y + b^2 / (g * 2) *)
    (* b = sqrt ((dest.y - src.y) * (g * 2)) *)
    let yd_yo = abs (dest.y -. src.y) in
    let v0 = sqrt (yd_yo *. g *. 2.0) in
    let y0 = src.y in
    {v0 = v0;
     y0 = y0}

  let compute_ordinate t time_param =
    t.y0 +. t.v0 *. time_param -. g *. time_param *. time_param /. 2.0

  let back_to_y0 t = 2.0 *. t.v0 /. g

end

module SymVerticalJump = struct
  let create src dest = VerticalJump.create (inv_y src) (inv_y dest)
  let compute_ordinate t time_param =
    0.0 -. (VerticalJump.compute_ordinate t time_param)
  let back_to_y0 t = VerticalJump.back_to_y0 t

end

module Parabolic = struct
  type t = {
    a:float;
    b:float;
    c:float;
  }

  let compute_between ?coeff:(c=0.5) xO xD =
    xO +. (xD -. xO) *. c

  let create ?coeff:(c=0.1) src dest =
    (* move from src to dest *)
    (* y = a * x * x + b * x + c *)
    (* lets choose the max abscisse *)
    let maxXCoeff =
      if src.y = dest.y then 0.5
      else c
    in
    let maxX = compute_between ~coeff:maxXCoeff src.x dest.x in
    (* src.y = a *. src.x *. src.x + b *. src.x + c *)
    (* dest.y = a *. dest.x *. dest.x + b *. dest.x + c *)
    (* replace c in dest.y with expression in src.y *)
    (* a * (src.x *. src.x - dest.x *. dest.x) = (src.y -. dest.y) - b. *. (src.x -. dest.x) *)
    (* maxX = - b /. (2 *. a) *)
    (* -maxX *. 2 *. a = b *)
    let a =
      if maxXCoeff <> 0.5 then begin
        (* a *. (src.x *. src.x - dest.x *. dest.x) - maxX *. 2 *. a * (src.x -. dest.x) = src.y - dest.y *)
        (* a = (src.y -. dest.y) / ((src.x *. src.x - dest.x *. dest.x) -maxX *. 2 *. (src.x -. dest.x) *)
        (src.y -. dest.y) /. ((src.x *. src.x -. dest.x *. dest.x) -. maxX *. 2.0 *. (src.x -. dest.x))
      end
      else begin
          (*we have an infinty of solutions so we choose yMax to be
            yMax = dest.y + upper
            using maxX on the general equation we have
            yMax = a *. maxX *. maxX +. (-2 *.maxX *.a) *. maxX +. c
            yMax = -a *. maxX^2 + c,
            uxing dest.x on the equation:
            c = dest.y - a * dest.x^2 + 2maxX * a * dest.x,
            we replace c,
            yMax = -a *. maxX^2 + dest.y - a * dest.x^2 + 2maxX * a * dest.x
            yMax - dest.y = a (2maxX * dest.x - maxX^2 - dest.x^2) *)
        let upper = (abs (src.x -. dest.x)) /. 5.0 in
        let yMax =  dest.y +. upper in
        (yMax -. dest.y) /. (2.0 *. maxX *. dest.x -. maxX *. maxX -. dest.x *. dest.x)
      end
    in
    let b = 0.0 -. a *. 2.0 *. maxX in
    let c = src.y -. b *. src.x -. a *. src.x *. src.x in
    {a=a;
     b=b;
     c=c}

  let compute_ordinate t x =
    t.a *. x *. x +. t.b *. x +. t.c
end

module BasketballTrajThatFitsTheScreen = struct
  type t = {
    f: float -> float;
  }

  let get_one_traj_f_and_max ?coeff:(coeff=0.1) src dest =
    let t  =
      if src.y <> dest.y then Parabolic.create ~coeff:coeff src dest
      else Parabolic.create src dest
    in
    let parabol_f= Parabolic.compute_ordinate t in
    if t.Parabolic.a <= 0.0 then
      (parabol_f,
       parabol_f ((0.0 -. t.Parabolic.b) /. (t.Parabolic.a *. 2.0)))
    else
      (* make the curve look good, by computing
         the symmetry from the line between the two points
      *)
      let lt = Line.create src dest in
      let line_f = Line.y_axis lt in
      let f x =
        let yparabol = parabol_f x in
        let yline = line_f x in
        2.0 *. yline -. yparabol
      in
      (* yparabol = a * x2 + b * x + c *)
      (* yline = s * x + r *)
      (* y = 2 * yline - yparabol *)
      (* y' = 2 * s - 2 * a * x - b *)
      (* 0 = 2 * s - 2 * a * max - b *)
      (* max = (2 * s - b)  / (2 * a) *)
      (f, f ((lt.Line.slope *. 2.0 -. t.Parabolic.b) /. (t.Parabolic.a *. 2.0)))

  let create src dest maxy =
     (* compute the one that goes the highest, but still fits into the screen *)
    let f, _ =
      if src.y <> dest.y then begin
       let get_max coeff =
         let _, max = get_one_traj_f_and_max ~coeff:coeff src dest in
         max
       in
       (* we browse for 100 trajectories the one that best suits our needs *)
       let r = List.map (fun x -> let coeff = 0.01 *. (float_of_int x) in
                                   (coeff, get_max coeff)) (List.filter (fun x -> x <> 50) (range 1 100)) in
       let filtered = List.filter (fun (coeff, max) -> max < maxy) r  in
       let (c, _) = List.hd (List.sort (fun (c1, max1) (c2, max2) -> Pervasives.compare max2 max1) filtered) in
       get_one_traj_f_and_max ~coeff:c src dest
      end
    else
      get_one_traj_f_and_max src dest
    in
    {f=f}

  let compute_ordinate t x_axis =
    t.f x_axis

end

module SymBasketball = struct
  let create src dest = BasketballTrajThatFitsTheScreen.create (inv_y src) (inv_y dest) 0.0

  let compute_ordinate t x_axis =
    0.0 -. (BasketballTrajThatFitsTheScreen.compute_ordinate t x_axis)

end
}}
