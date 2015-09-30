{server{

let update_preferred_lang (id, lang) = Db.User.update_preferred_lang id lang
let get_translations (id, lang) = Db.Translation.get_translations id lang
let set_translation (lword, rword, descr, rlang, user_id) = Db.Translation.set lword rword rlang ~description:descr user_id
let unset_translation (lword, rword, lang, user_id) = Db.Translation.unset lword rword lang user_id

let rpc_update_preferred_lang =
  server_function Json.t<int32 * string> update_preferred_lang

let rpc_get_supported_lang =
  server_function Json.t<unit> Db.LangDb.get_supported_languages

let rpc_get_translations =
  server_function Json.t<int32 * string> get_translations

let rpc_set_translation =
  server_function Json.t<string * string * string * string * int32> set_translation

let rpc_unset_translation =
  server_function Json.t<string * string * string * int32> unset_translation

}}

{client{

let update_translations model =
  let arg = Edit_dictionary_model.(get_user_id model, get_preferred_lang model) in
  lwt translations = %rpc_get_translations arg in
  let trans = List.map Edit_dictionary_model.create_translation translations in
  Lwt.return (Edit_dictionary_model.update_translations model trans)

let update_preferred_lang f model preferred_lang =
  lwt () = %rpc_update_preferred_lang Edit_dictionary_model.(get_user_id model, preferred_lang) in
  let model = Edit_dictionary_model.update_preferred_lang model preferred_lang in
  lwt new_model = update_translations model in
  let () = f new_model in
  Lwt.return_unit

let change_preferred_lang f model =
  lwt languages = %rpc_get_supported_lang () in
  let () = f (Edit_dictionary_model.change_preferred_lang model languages) in
  Lwt.return_unit

let back_to_init f model =
  let () = f (Edit_dictionary_model.back_to_init model) in
  Lwt.return_unit

let delete_in_translation_in_db model translation =
  let user_id, preferred_lang = Edit_dictionary_model.(get_user_id model, get_preferred_lang model) in
  let trans = Edit_dictionary_model.(get_translation translation) in
  lwt () = %rpc_unset_translation Utils.Translation.(trans.source, trans.dest, preferred_lang, user_id) in
  Lwt.return (Edit_dictionary_model.delete_translation_from_model model translation)

let add_translation f ?oldval:(oval=None) model source dest description =
  lwt model =
    match oval with
    | None -> Lwt.return model
    | Some v -> delete_in_translation_in_db model v
  in
  let user_id, preferred_lang = Edit_dictionary_model.(get_user_id model, get_preferred_lang model) in
  lwt new_model =
    match_lwt (%rpc_set_translation (source, dest, description, preferred_lang, user_id)) with
  | true -> let new_translation = Edit_dictionary_model.create_translation
                                    Utils.Translation.({source; dest; description}) in
            Lwt.return (Edit_dictionary_model.add_translation model new_translation)
  | false -> Lwt.return (Edit_dictionary_model.set_translation_error model)
  in
  let () = f new_model in
  Lwt.return_unit

let del_translation f model translation =
  lwt new_model = delete_in_translation_in_db model translation in
  let () = f new_model in
  Lwt.return_unit

let clear_error f model =
  let new_model = Edit_dictionary_model.set_translation_ok model in
  let () = f new_model in
  Lwt.return_unit

let edit_translation f model translation =
  let new_model = Edit_dictionary_model.set_translation_as_edit model translation in
  let () = f new_model in
  Lwt.return_unit

let cancel_edit_translation f model translation =
  let new_model = Edit_dictionary_model.set_translation_as_read model translation in
  let () = f new_model in
  Lwt.return_unit

let back_to_learning f model =
  let new_model = Edit_dictionary_model.back_to_learning model in
  let () = f new_model in
  Lwt.return_unit

let goto_play f model =
  let new_model = Edit_dictionary_model.goto_play model in
  let () = f new_model in
  Lwt.return_unit

}}
