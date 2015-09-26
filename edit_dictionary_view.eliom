{client{
  open Tyxml_js

  let input_value i = Js.to_string (To_dom.of_select i) ## value

  let preferred_lang_to_string x =
    match x with
    | "en" -> "ingelesa"
    | "ru" -> "errusiera"
    | "fr" -> "frantsesa"
    | _ -> assert false

  let string_to_preferred_lang x =
    match x with
    | "ingelesa" -> "en"
    | "errusiera" -> "ru"
    | "frantsesa" -> "fr"
    | _ -> assert false

  let preferred_lang_to_string_declined x =
    match x with
    | "en" -> "ingelesez"
    | "ru" -> "errusieraz"
    | "fr" -> "frantsesez"
    | _ -> assert false

  let view_content f model =
    let open Edit_dictionary_model in
    let lang = Edit_dictionary_model.(model.current_user.Current_user.preferred_lang) in
    let str = Printf.sprintf "Zure ama hizkuntza %s omen da. " (preferred_lang_to_string lang) in
    let banner = Html5.(strong [pcdata str]) in
    let under_banner =
     match model.state with
    | `Unit ->
       let button = Utils.create_button `Goto
                                        "Ez, beste bat da nire ama hizkuntza"
                                        (fun () -> Edit_dictionary_controller.change_preferred_lang f model) in
       [button]
    | `Change_preferred_lang l ->
       let select = Html5.(select (List.map (fun x -> option (pcdata (preferred_lang_to_string x))) l)) in
       let c_button = Utils.create_button `Goto
                                        "Utzi"
                                        (fun () -> Edit_dictionary_controller.back_to_init f model) in


       let u_button = Utils.create_button `ActionLittle
                                          "Eguneratu"
                                          (fun () ->
                                           let input_val = input_value select in
                                           Edit_dictionary_controller.update_preferred_lang f model (string_to_preferred_lang input_val)) in

       [Html5.(pcdata "Nire benetako ama hizkuntza ");
        select;
        Html5.(pcdata " da.");
        u_button;
        c_button]
    in
    let translations = Utils.create_table ["euskaraz"; preferred_lang_to_string_declined (get_preferred_lang model); "azalpenak"]
                                          (List.map (fun x -> Utils.Translation.([x.source; x.dest; x.description]))
                                                    (get_translations model)) in
    let under_banner = under_banner @ [translations] in
    Html5.(banner :: under_banner)

  let view ((r, f): Edit_dictionary_model.rp) =
    R.Html5.(div (Utils.ReactList.list (React.S.map (view_content f) r)))

  let create_auth_page user =
    let model = Edit_dictionary_model.create user in
    let r, f = React.S.create model in
    let res = view (r, f) in
    let () = Lwt_js_events.(async (fun () ->
                                   lwt _ = onload () in
                                   lwt translations = Edit_dictionary_controller.get_translations user in
                                   let model = Edit_dictionary_model.update_translations model translations in
                                   let () = f model in
                                   Lwt.return_unit)) in
    res


}}
