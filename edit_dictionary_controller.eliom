{server{

let update_preferred_lang (id, lang) = Db.User.update_preferred_lang id lang
let get_translations (id, lang) = Db.Translation.get_translations id lang
let set_translation (lword, rword, descr, rlang, user_id) = Db.Translation.set lword rword rlang ~description:descr user_id

let rpc_update_preferred_lang =
  server_function Json.t<int32 * string> update_preferred_lang

let rpc_get_supported_lang =
  server_function Json.t<unit> Db.LangDb.get_supported_languages

let rpc_get_translations =
  server_function Json.t<int32 * string> get_translations

let rpc_set_translation =
  server_function Json.t<string * string * string * string * int32> set_translation

}}

{client{

let update_translations user =
  let arg = Edit_dictionary_model.(get_user_id user, get_preferred_lang user) in
  lwt translations = %rpc_get_translations arg in
  Lwt.return (Edit_dictionary_model.update_translations user translations)

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

let add_translation f model source dest description =
  let user_id, preferred_lang = Edit_dictionary_model.(get_user_id model, get_preferred_lang model) in
  lwt () = %rpc_set_translation (source, dest, description, preferred_lang, user_id) in
  let () = f (Edit_dictionary_model.add_translation model Utils.Translation.({source; dest; description})) in
  Lwt.return_unit
}}
