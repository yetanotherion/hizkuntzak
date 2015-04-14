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

module Line = struct
  type t = {
    slope: float;
    origin: float;
  }

  let create src dest =
    let () =
      if dest.x == src.x then failwith("src and dest must have different x-labels")
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
  let inv_y p = {y=0.0 -. p.y;
                 x=p.x}

  let create src dest = VerticalJump.create (inv_y src) (inv_y dest)
  let compute_ordinate t time_param =
    0.0 -. (VerticalJump.compute_ordinate t time_param)
  let back_to_y0 t = VerticalJump.back_to_y0 t

end
}}
