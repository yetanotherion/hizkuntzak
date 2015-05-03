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
{shared{
open Eliom_lib
open Eliom_content
open Html5.D

let must x =
  match x with
    | None -> assert(false)
    | Some x -> x

let max_of_non_empty_list l =
  List.fold_left
    (fun accum elt ->
      Pervasives.max accum elt)
    (List.hd l)
    (List.tl l)

let list_to_select s l =
  let list = List.map (fun name -> Option ([], name, Some (pcdata name), true)) l in
  let head = Option ([], s, Some (pcdata s), true)  in
  head, list

let build_raw_select name default others =
  let h, l = list_to_select default others in
  raw_select ~a:[] ~required:(pcdata "") ~name:name h l

}}

{server{
let create_bootstrap_head () =
  let utf8_meta = meta ~a:[a_charset "utf8"] () in
  let viewport_meta = meta ~a:[a_name "viewport";
                               a_content "width=device-width, initial-scale=1"] () in
  let href_link = uri_of_string (fun () -> "http://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css") in
  let stylesheet = link ~rel:[`Stylesheet] ~href:href_link () in
  let make_script x = script ~a:[a_src (uri_of_string (fun () -> x))] (pcdata "") in
  let js_scripts = List.map make_script ["https://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js";
                                         "http://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min.js"] in
  [utf8_meta;
   viewport_meta;
   stylesheet] @ js_scripts

}}

{client{
  let log s = Firebug.console##log(Js.string s)

  let get_input_text element = Js.to_string element##value

  let write_in_input input str =
    input##value <- Js.string str

  let hidde_element elt =
    if not (Js.to_bool (elt##classList##contains(Js.string "hidden")))
    then elt##classList##add(Js.string "hidden")

  let show_element elt =
    if (Js.to_bool (elt##classList##contains(Js.string "hidden")))
    then elt##classList##remove(Js.string "hidden")
}}
