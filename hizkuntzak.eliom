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
      Lwt.return
        (Eliom_tools.F.html
           ~title:"hizkuntzak"
           ~css:[["css";"hizkuntzak.css"]]
           Html5.F.(body [
             h2 [pcdata "Welcome from Eliom's distillery!"];
           ])))
