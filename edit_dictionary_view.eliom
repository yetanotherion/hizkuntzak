{client{
  open Tyxml_js

  let input_value i = Js.to_string (To_dom.of_select i) ## value
  let view_content f model =
    let open Edit_dictionary_model in
    let str = Printf.sprintf "Aukeratutako hizkuntza: %s" Edit_dictionary_model.(model.current_user.Current_user.preferred_lang) in
    let banner = Html5.(h3 [pcdata str]) in
    let under_banner =
     match model.state with
    | `Unit ->
       let button = Utils.create_button `Goto
                                        "Eguneratu"
                                        (fun () -> Edit_dictionary_controller.change_preferred_lang f model) in
       [button]
    | `Change_preferred_lang l ->
       let select = Html5.(select (List.map (fun x -> option (pcdata x)) l)) in
       let c_button = Utils.create_button `Goto
                                        "Utzi"
                                        (fun () -> Edit_dictionary_controller.back_to_init f model) in


       let u_button = Utils.create_button `ActionLittle
                                          "Eguneratu"
                                          (fun () ->
                                           Edit_dictionary_controller.update_preferred_lang f model (input_value select)) in

       [Html5.(h4 [pcdata "Hizkuntza berria:"]);
        select;
        u_button;
        c_button]
    in
    let translations = Utils.create_table ["eus"; get_preferred_lang model; "azalpenak"]
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
