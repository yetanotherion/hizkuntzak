{server{
open Eliom_content

let service unused unused_bis =
  let otherh = Utils.create_bootstrap_head () in
  let b = Html5.F.(body ~a:[a_id "main"] []) in
  let _ = {unit Lwt.t{ Auth_view.setup Edit_dictionary.create_auth_page }} in
  let res = Eliom_tools.F.html ~title:"improve your dictionary" ~css:[["css";"hizkuntzak.css"]] ~other_head:otherh b in
  Lwt.return res
}}
