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

      Lwt.return
        (Eliom_tools.F.html
           ~title:"hizkuntzak"
           ~css:[["css";"hizkuntzak.css"]]
           ~other_head:h
           Html5.F.(body [
             h2 [pcdata "Welcome from Eliom's distillery!"];
           ])))
