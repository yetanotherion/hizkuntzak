{client{
  open Tyxml_js

  let preferred_lang_to_string x =
    match x with
    | "en" -> "ingelesa"
    | "ru" -> "errusiera"
    | "fr" -> "frantsesa"
    | "es" -> "gaztelera"
    | "eus" -> "euskara"
    | _ -> assert false

  let string_to_preferred_lang x =
    match x with
    | "ingelesa" -> "en"
    | "errusiera" -> "ru"
    | "frantsesa" -> "fr"
    | "gaztelera" -> "es"
    | "euskara" -> "eus"
    | _ -> assert false

  let preferred_lang_to_string_declined x =
    match x with
    | "en" -> "ingelesez"
    | "ru" -> "errusieraz"
    | "fr" -> "frantsesez"
    | "es" -> "gazteleraz"
    | "eus" -> "euskaraz"
    | _ -> assert false

  let create_edition_ui f model state data translation =
    match state with
    | `Read ->
       let button = Utils.create_button `ActionLittle
                                        "Aldatu"
                                        (fun () ->
                                         Edit_dictionary_controller.(
                                           edit_translation
                                           f
                                           model
                                           translation)) in
        Utils.TranslationInModel.([[Html5.pcdata data.source];
                                   [Html5.pcdata data.dest];
                                   [Html5.pcdata data.description;
                                    Html5.br ();
                                    button]])
     | `Edit ->
        let cn = Utils.create_input ~name_for_placeholder:false in
        let (source, dest, description), id = Utils.TranslationInModel.(
            (cn data.source, cn data.dest, cn data.description),
            data.id) in
        let edit_button = Utils.create_button `ActionLittle
                                              "Aldatu"
                                              (fun () ->
                                               let i = Utils.input_value in
                                               let s,
                                                   dst,
                                                   descr = i source,
                                                           i dest,
                                                           i description in
                                               Edit_dictionary_controller.(
                                                 update_translation f
                                                                    model
                                                                    id
                                                                    s
                                                                    dst
                                                                    descr)) in
        let delete_button = Utils.create_button `ActionLittleRed
                                              "Kendu"
                                              (fun () ->
                                               Edit_dictionary_controller.(
                                                 del_translation
                                                   f
                                                   model
                                                   translation)) in
        let cancel_button = Utils.create_button `Goto
                                                "Utzi"
                                                (fun () ->
                                                 Edit_dictionary_controller.(
                                                   cancel_edit_translation
                                                     f
                                                     model
                                                     translation)) in

        [[source];
         [dest];
         [description;
          edit_button;
          delete_button;
          cancel_button]]

  let original_ui f model translation original =
    let open Edit_dictionary_model in
    let data, state, correction_state =
      Translation.Original.(
        original.data,
        original.state,
        original.correction_state) in
    create_edition_ui f model state data translation

  let correction_ui f model translation correction =
    let open Edit_dictionary_model in
    let data, state = Translation.Correction.(
        correction.data,
        correction.state) in
    create_edition_ui f model state data translation

  let translation_ui f model translation =
    let open Edit_dictionary_model in
    match translation with
    | `Original x -> original_ui f model translation x
    | `Correction x -> correction_ui f model translation x

 let create_src_or_dst_lang_ui f model src_or_dst =
    let open Edit_dictionary_model in
    let lang, state =
      match src_or_dst with
      | `Src -> get_preferred_lang_src model,
                model.state.src_preferred_lang_state
      | `Dst -> get_preferred_lang_dst model,
                model.state.dst_preferred_lang_state
    in
    let lang = preferred_lang_to_string_declined lang in
    match state with
    | `Unit ->
       let button = Utils.create_button `Goto
                                        "Ez, beste hizkuntza bat nahi dut"
                                        (fun () ->
                                         Edit_dictionary_controller.(
                                           change_preferred_lang
                                             f
                                             model
                                             src_or_dst)) in
       [Html5.pcdata (lang ^ " ");
        button]
    | `Change_preferred_lang l ->
       let select = Utils.create_select (List.map
                                           (fun x ->
                                            (preferred_lang_to_string x)) l) in
       let c_button = Utils.create_button `Goto
                                          "Utzi"
                                          (fun () ->
                                           Edit_dictionary_controller.(
                                             back_to_init f
                                                          model
                                                          src_or_dst)) in


       let u_button = Utils.create_button `ActionLittle
                                          "Eguneratu"
                                          (fun () ->
                                           let input_val =
                                             Utils.select_value select in
                                           Edit_dictionary_controller.(
                                             update_preferred_lang
                                               f
                                               model
                                               src_or_dst
                                               (string_to_preferred_lang
                                                  input_val))) in

       [Html5.pcdata "Nik ";
        select;
        Html5.pcdata " nahi dut.";
        u_button;
        c_button]

 let create_table_header f model =
   let src_ui = create_src_or_dst_lang_ui f model `Src in
   let dst_ui = create_src_or_dst_lang_ui f model `Dst in
   Html5.([src_ui;
           dst_ui;
           [pcdata "azalpenak"]])

 let view_play f model =
   let div = Html5.div [] in
   let button = Utils.create_button `ActionLittle
                                    "Hiztegira itzuli"
                                    (fun () ->
                                     Edit_dictionary_controller.back_to_learning
                                       f
                                       model) in
   let pairs = Edit_dictionary_model.get_pairs model in
   let () = Dictionary_game.Client.create_and_setup (To_dom.of_div div) pairs in
   [button; div]

 let view_learn f model =
   let open Edit_dictionary_model in
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
    let under_banner = add_translation_state in
    let play_button = Utils.create_button `ActionLittle
                                          "Jolasten hasi"
                                          (fun () ->
                                           Edit_dictionary_controller.goto_play f model) in

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
    let translations = Utils.create_table (create_table_header f model)
                                          (add_translation :: existing) in
    under_banner @ [play_button; translations]

 let view_content f model =
   let open Edit_dictionary_model in
   match model.global_state with
   | `Learn -> view_learn f model
   | `Play -> view_play f model

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
