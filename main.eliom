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

let () =
  let eus = Hizkuntzak_app.register_service ["eus"] Eliom_parameter.unit Eus.service in
  let eus_dict = Hizkuntzak_app.register_service ["hiztegia"] Eliom_parameter.unit Dictionary.service in
  let eus_taulak = Hizkuntzak_app.register_service ["eus_taulak"] Eliom_parameter.unit Eus_taulak.service in
  let main_service =
    Eliom_service.App.service ~path:[] ~get_params:Eliom_parameter.unit ()
  in

  Hizkuntzak_app.register
    ~service:main_service
    (fun () () ->
      let links = ul
       [li ~a:[a_class ["active"]] [a ~service:eus [pcdata "Games on basque language"] ()];
        li ~a:[a_class ["active"]] [a ~service:eus_dict [pcdata "Games based on the dictionary you create little by little"] ()]]
      in
      let courses = ul
       [li ~a:[a_class ["active"]] [a ~service:eus_taulak [pcdata "Basque Verb tabulars"] ()]]
      in
      Lwt.return
        (Eliom_tools.F.html
           ~title:"hizkuntzak"
           ~css:[["css";"hizkuntzak.css"]]
           ~other_head:(Utils.create_bootstrap_head ())
           Html5.F.(body [
             h2 [pcdata "Available games:"];
             div [links];
             h2 [pcdata "Available courses:"];
             div [courses]
           ])))
