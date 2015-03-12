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
}}

module Hizkuntzak_app =
  Eliom_registration.App (
    struct
      let application_name = "hizkuntzak"
    end)

{client{

type t = {
  nor_input: Dom_html.inputElement Js.t;
  nork_input: Dom_html.inputElement Js.t;
  time_input: Dom_html.inputElement Js.t;
  output: Dom_html.inputElement Js.t;
}

let create nor_input nork_input time_input output =
  {
    nor_input = Html5.To_dom.of_input nor_input;
    nork_input = Html5.To_dom.of_input nork_input;
    time_input = Html5.To_dom.of_input time_input;
    output = Html5.To_dom.of_input output;
  }

let get_input_text element = Js.to_string element##value

let conjugate t =
  let nor = get_input_text t.nor_input in
  let nork = get_input_text t.nork_input in
  let time_input = get_input_text t.time_input in
  let time =
    match time_input with
      | "oraina" -> `Present
      | "iragana" -> `Past
      | "" -> `Present
      | x -> raise (Failure ("invalid time set 'oraina' or 'iragana': " ^ x))
  in
  let nor_mode =
    match nor with
      | "ni" -> `Ni
      | "hi" -> `Hi
      | "hura" -> `Hura
      | "gu" -> `Gu
      | "zu" -> `Zu
      | "zuek" -> `Zuek
      | "haiek" -> `Haiek
      | x -> raise (Failure ("invalid nor mode: " ^ x))
  in
  let nork_mode =
    match nork with
      | "nik" -> Some `Nik
      | "hik" -> Some (`Hik (`Male))
      | "hark" -> Some `Hark
      | "guk" -> Some `Guk
      | "zuk" -> Some `Zuk
      | "zuek" -> Some `Zuek
      | "haiek" -> Some `Haiek
      | "" -> None
      | x -> raise (Failure ("invalid nork mode: " ^ x))
  in
  let param =
    match nork_mode with
      | None -> `Nor nor_mode
      | Some nork -> `NorNork (nor_mode, nork)
  in
  Tables.conjugate param time

let on_any_input_changes t _ _ =
  let message =
    try
      conjugate t
    with Failure s -> s
  in
  t.output##value <- Js.string message;
  Lwt.return_unit

}}


let main_service =
  Eliom_service.App.service ~path:[] ~get_params:Eliom_parameter.unit ()

let () =
  Hizkuntzak_app.register
    ~service:main_service
    (fun () () ->
        let utf8_meta = meta ~a:[a_charset "utf8"] () in
        let viewport_meta = meta ~a:[a_name "viewport";
                                     a_content "width=device-width, initial-scale=1"] () in
        let href_link = uri_of_string (fun () -> "http://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css") in
        let stylesheet = link ~rel:[`Stylesheet] ~href:href_link () in
        let make_script x = script ~a:[a_src (uri_of_string (fun () -> x))] (pcdata "") in
        let js_scripts = List.map make_script ["https://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js";
                                               "http://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min.js"] in
        let h = [utf8_meta;
                 viewport_meta;
                 stylesheet] @ js_scripts in
        let nor_input = string_input ~input_type:`Text () in
        let nork_input = string_input ~input_type:`Text () in
        let time_input = string_input ~input_type:`Text () in
        let output = string_input ~input_type:`Text () in
        let _ = {unit{
          let t = create %nor_input %nork_input %time_input %output in
          let open Lwt_js_events in
          async (fun () ->
            changes t.nor_input (on_any_input_changes t));
          async (fun () ->
            changes t.nork_input (on_any_input_changes t));
          async (fun () ->
            changes t.time_input (on_any_input_changes t));
        }}
        in

          Lwt.return
        (Eliom_tools.F.html
           ~title:"hizkuntzak"
           ~css:[["css";"hizkuntzak.css"]]
           ~other_head:h
           Html5.F.(body [
             h2 [pcdata "Welcome from Eliom's distillery!"];
             div [div [pcdata "nor"; nor_input];
                  div [pcdata "nork"; nork_input];
                  div [pcdata "denbora"; time_input];
                  div [pcdata "="; output]]
           ])))
