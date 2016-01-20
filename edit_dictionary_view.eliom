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
        let source_lang, dest_lang = Utils.TranslationInModel.(data.source_lang, data.dest_lang) in
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
                                                                    source_lang
                                                                    dst
                                                                    dest_lang
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

  let append_to_nth_element l elements n =
    let one, two, three = List.nth l 0,
                          List.nth l 1,
                          List.nth l 2 in
    match n with
    | `One -> [one @ elements; two; three]
    | `Two -> [one; two @ elements; three]
    | `Three -> [one; two; three @ elements]

  let replace_nth_element l elements n =
    let one, two, three = List.nth l 0,
                          List.nth l 1,
                          List.nth l 2 in
    match n with
    | `One -> [elements; two; three]
    | `Two -> [one; elements; three]
    | `Three -> [one; two; elements]

  let append_to_fst_element l elements =
    append_to_nth_element l elements `One

  let append_to_snd_element l elements =
    append_to_nth_element l elements `Two

  let append_to_third_element l elements =
    append_to_nth_element l elements `Three

  let replace_third_element l elements =
    replace_nth_element l elements `Three

  let original_ui f model translation original =
    let open Edit_dictionary_model in
    let data, state, correction_state =
      Translation.Original.(
        original.data,
        original.state,
        original.correction_state) in
    let edition_buttons = create_edition_ui f model state data translation in
    let cn = Utils.create_input ~name_for_placeholder:false in
    match correction_state with
    | `None ->
       let button = Utils.create_button `Goto
                                        "Zuzenketa galdetu"
                                        (fun () ->
                                         Edit_dictionary_controller.(
                                           set_to_choosing_corrector
                                             f
                                             model
                                             original
                                             ""))
       in
       append_to_third_element edition_buttons [Html5.br ();
                                                button]
    | `ChoosingCorrector s ->
       let corrector = cn s in
       let edit_button = Utils.create_button `ActionLittle
                                             "Hautatu"
                                             (fun () ->
                                              let i = Utils.input_value in
                                              let corrector = i corrector in
                                              Edit_dictionary_controller.(
                                                set_corrector
                                                  f
                                                  model
                                                  original
                                                  corrector)) in
       let cancel_button = Utils.create_button `ActionLittleRed
                                               "Kendu"
                                               (fun () ->
                                                Edit_dictionary_controller.(
                                                  set_to_no_corrector
                                                    f
                                                    model
                                                    original)) in
       append_to_third_element edition_buttons [Html5.br ();
                                                corrector;
                                                edit_button;
                                                cancel_button]
    | `CorrectorDoesNotExist corrector ->
       let msg = Html5.pcdata (Printf.sprintf
                                 "%s erabiltzailerik ez dago"
                                 corrector) in
       let ok = Utils.create_button `ActionLittle
                                    "Ados"
                                    (fun () ->
                                     Edit_dictionary_controller.(
                                       set_to_choosing_corrector
                                         f
                                         model
                                         original
                                         corrector)) in
       append_to_third_element edition_buttons [Html5.br ();
                                                msg;
                                                ok]
    | `CorrectorChosen x ->
       let username = Edit_dictionary_model.Translation.get_username x in
       let msg = Html5.pcdata (Printf.sprintf
                                 "%s erabiltzaileari zuzenketa galdetua"
                                 username) in
       append_to_third_element edition_buttons [Html5.br ();
                                                msg]
    | `CorrectionDone data ->
       let ok = Utils.create_button `ActionLittle
                                    "Zuzenketa aplikatu"
                                    (fun () ->
                                     Edit_dictionary_controller.(
                                       acknowledge_validated_correction
                                         f
                                         model
                                         original
                                         data)) in
       let nok = Utils.create_button `ActionLittleRed
                                    "Zuzenketa baztertu"
                                    (fun () ->
                                     Edit_dictionary_controller.(
                                       cancel_validated_correction
                                         f
                                         model
                                         original
                                         data)) in
       let open Utils.TranslationInModel in
       let make_correction x = Html5.(div ~a:[a_style "color:red"] [pcdata x]) in
       let third = List.nth edition_buttons 2 in
       let third = [make_correction data.description; Html5.br ()] @ third @ [ok; nok] in
       let res = replace_third_element edition_buttons third in
       let res = append_to_fst_element res [make_correction data.source] in
       append_to_snd_element res [make_correction data.dest]

  let correction_ui f model translation correction =
    let open Edit_dictionary_model in
    let data, state, original = Translation.Correction.(
        correction.data,
        correction.state,
        correction.original) in
    let edition_buttons = create_edition_ui f model state data translation in
    let original_username = Translation.get_username original in
    let ok = Utils.create_button `Goto
                                 "Zuzenketa Bidali"
                                 (fun () ->
                                     Edit_dictionary_controller.(
                                       validate_correction
                                         f
                                         model
                                         correction)) in
    let ui = append_to_third_element edition_buttons
                                     [ok] in
    [Html5.pcdata original_username] :: ui

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

 let create_correction_table_header src_lang dst_lang =
   let pl = preferred_lang_to_string_declined in
   Html5.([[pcdata "Nor"];
           [pcdata (pl src_lang)];
           [pcdata (pl dst_lang)];
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
    let translations = get_translations model in
    let originals, corrections = List.fold_left
                                  (fun (originals, corrections) arg ->
                                   let open Edit_dictionary_model in
                                   match arg with
                                   | `Original _ -> (arg :: originals), corrections
                                   | `Correction x -> originals, (x, arg) :: corrections) ([], [])
                                  translations in
    let h = Hashtbl.create 100 in
    let () = List.iter (fun x ->
                        let correction, _ = x in
                        let open Edit_dictionary_model in
                        let open Translation.Correction in
                        let key = Utils.TranslationInModel.(correction.data.source_lang,
                                                            correction.data.dest_lang) in
                        let l =
                          if Hashtbl.mem h key then
                            Hashtbl.find h key
                          else []
                        in
                        Hashtbl.replace h key (x :: l)
                       ) corrections
    in
    let corrections_table = Hashtbl.fold
                              (fun k value accum ->
                               let src, dst = k in
                               let ch = create_correction_table_header src dst in
                               let ui = List.map
                                          (fun (_, transl) ->
                                           translation_ui f model transl) value in
                               (Utils.create_table ch ui) :: accum) h [] in
    let () = Hashtbl.reset h in
    let existing = List.map (fun trans -> translation_ui f model trans)
                            originals in
    let translations = Utils.create_table (create_table_header f model)
                                          (add_translation :: existing) in
    under_banner @ (play_button :: corrections_table) @ [translations]

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
