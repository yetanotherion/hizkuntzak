{server{

let update_preferred_lang (id, lang) = Db.User.update_preferred_lang id lang
let get_translations (id, lang) = Db.Translation.get_translations id lang

let rpc_update_preferred_lang =
  server_function Json.t<int32 * string> update_preferred_lang

let rpc_get_supported_lang =
  server_function Json.t<unit> Db.LangDb.get_supported_languages

let rpc_get_translations =
  server_function Json.t<int32 * string> get_translations
}}

{client{

let update_preferred_lang f model preferred_lang =
  let open Edit_dictionary_model in
  lwt () = %rpc_update_preferred_lang (get_user_id model, preferred_lang) in
  let () = f (update_preferred_lang model preferred_lang) in
  Lwt.return_unit

let change_preferred_lang f model =
  lwt languages = %rpc_get_supported_lang () in
  let () = f (Edit_dictionary_model.change_preferred_lang model languages) in
  Lwt.return_unit

let back_to_init f model =
  let () = f (Edit_dictionary_model.back_to_init model) in
  Lwt.return_unit

let get_translations user =
  let arg = Current_user.(get_user_id user, get_preferred_lang user) in
  %rpc_get_translations arg
}}
