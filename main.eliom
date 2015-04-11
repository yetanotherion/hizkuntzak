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
  let eus_howto = Hizkuntzak_app.register_service ["eus_howto"] Eliom_parameter.unit Eus_aditzak.service in
  let ru = Hizkuntzak_app.register_service ["ru"] Eliom_parameter.unit Ru.service in
  let main_service =
    Eliom_service.App.service ~path:[] ~get_params:Eliom_parameter.unit ()
  in

  Hizkuntzak_app.register
    ~service:main_service
    (fun () () ->
      let links = ul
        [li ~a:[a_class ["active"]] [a ~service:eus [pcdata "Games on basque language"] ()];
         li ~a:[a_class ["active"]] [a ~service:eus_howto [pcdata "How verbs work on basque language"] ()];
         li ~a:[a_class ["active"]] [a ~service:ru [pcdata "Games on russian language"] ()];  ]
      in
      Lwt.return
        (Eliom_tools.F.html
           ~title:"hizkuntzak"
           ~css:[["css";"hizkuntzak.css"]]
           ~other_head:(Utils.create_bootstrap_head ())
           Html5.F.(body [
             h2 [pcdata "Available games:"];
             div [links]
           ])))
