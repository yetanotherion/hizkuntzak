{client{
  open Tyxml_js

  let preferred_lang_to_string x =
    match x with
    | "en" -> "ingelesa"
    | "ru" -> "errusiera"
    | "fr" -> "frantsesa"
    | "es" -> "gaztelera"
    | _ -> assert false

  let string_to_preferred_lang x =
    match x with
    | "ingelesa" -> "en"
    | "errusiera" -> "ru"
    | "frantsesa" -> "fr"
    | "gaztelera" -> "es"
    | _ -> assert false

  let preferred_lang_to_string_declined x =
    match x with
    | "en" -> "ingelesez"
    | "ru" -> "errusieraz"
    | "fr" -> "frantsesez"
    | "es" -> "gazteleraz"
    | _ -> assert false

  let translation_ui f model translation =
    let open Edit_dictionary_model in
    let x = Translation.(translation.value) in
    match Translation.(translation.state) with
    | `Read ->
       let button = Utils.create_button `ActionLittle
                                        "Aldatu"
                                        (fun () ->
                                         Edit_dictionary_controller.edit_translation f
                                                                                     model
                                                                                     translation) in
        Utils.Translation.([[Html5.pcdata x.source];
                            [Html5.pcdata x.dest];
                            [Html5.pcdata x.description;
                             Html5.br ();
                             button]])
     | `Edit ->
        let cn = Utils.create_input ~name_for_placeholder:false in
        let source, dest, description = Utils.Translation.(cn x.source, cn x.dest, cn x.description) in
        let edit_button = Utils.create_button `ActionLittle
                                              "Gehitu"
                                              (fun () ->
                                               let i = Utils.input_value in
                                               let s, dst, descr = i source, i dest, i description in
                                               Edit_dictionary_controller.add_translation ~oldval:(Some translation) f
                                                                                          model
                                                                                          s dst descr) in
        let delete_button = Utils.create_button `ActionLittleRed
                                              "Kendu"
                                              (fun () ->
                                               Edit_dictionary_controller.del_translation f
                                                                                          model
                                                                                          translation) in
        let cancel_button = Utils.create_button `Goto
                                                "Utzi"
                                                (fun () ->
                                                 Edit_dictionary_controller.cancel_edit_translation f
                                                                                                    model
                                                                                                    translation) in

        Utils.Translation.([[source];
                            [dest];
                            [description;
                             edit_button;
                             delete_button;
                             cancel_button]])

 let view_content f model =
    let open Edit_dictionary_model in
    let lang = Edit_dictionary_model.(model.current_user.Current_user.preferred_lang) in
    let str = Printf.sprintf "Zure ama hizkuntza %s da. " (preferred_lang_to_string lang) in
    let banner = Html5.(strong [pcdata str]) in
    let under_banner =
     match model.state.preferred_lang_state with
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
                                           let input_val = Utils.select_value select in
                                           Edit_dictionary_controller.update_preferred_lang f model (string_to_preferred_lang input_val)) in

       [Html5.(pcdata "Nire benetako ama hizkuntza");
        select;
        Html5.(pcdata " da.");
        u_button;
        c_button]
    in
    let add_translation_state =
      match model.state.add_translation_state with
      | `Ok -> []
      | `Duplicated_translation ->
         let ok_button = Utils.create_button `ActionLittle
                                             "Ados ulertu dut"
                                             (fun () ->
                                              Edit_dictionary_controller.clear_error f model) in
         Html5.([br ();
                 pcdata "Barkatu baina gehitu nahi zenuen sarrera jadanik dago.";
                 ok_button])
    in
    let under_banner = under_banner @ add_translation_state in
    let source, dest, description = Utils.(create_input "hitz berria", create_input "itzulpena", create_input "azalpena") in
    let update = Utils.create_button `ActionLittle
                                     "Gehitu"
                                     (fun () ->
                                      let source, dest, value = Utils.(input_value source,
                                                                       input_value dest,
                                                                       input_value description) in
                                      Edit_dictionary_controller.add_translation f
                                                                                 model
                                                                                 source
                                                                                 dest
                                                                                 value) in
    let add_translation = [[source]; [dest]; [description; update]] in
    let existing = List.map (fun trans -> translation_ui f model trans)
                            (get_translations model) in
    let translations = Utils.create_table ["euskaraz";
                                           preferred_lang_to_string_declined (get_preferred_lang model);
                                           "azalpenak"]
                                          (add_translation :: existing) in
    let under_banner = under_banner @ [translations] in
    Html5.(banner :: under_banner)

  let view ((r, f): Edit_dictionary_model.rp) =
    R.Html5.(div (Utils.ReactList.list (React.S.map (view_content f) r)))

  let create_auth_page user =
    let model = Edit_dictionary_model.create user in
    let r, f = React.S.create model in
    let res = view (r, f) in
    let () = Lwt_js_events.(async (fun () ->
                                   lwt model = Edit_dictionary_controller.update_translations model in
                                   let () = f model in
                                   Lwt.return_unit)) in
    res


}}
