{shared{
open Eliom_lib
open Eliom_content
open Html5.D
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
  let get_input_text element = Js.to_string element##value

  let hidde_element elt =
    if not (Js.to_bool (elt##classList##contains(Js.string "hidden")))
    then elt##classList##add(Js.string "hidden")

  let show_element elt =
    if (Js.to_bool (elt##classList##contains(Js.string "hidden")))
    then elt##classList##remove(Js.string "hidden")

}}
