{server{

let update_preferred_lang_src (id, lang) =
  Db.User.update_preferred_lang_src id lang
let update_preferred_lang_dst (id, lang) =
  Db.User.update_preferred_lang_dst id lang
let get_users user_ids =
  Db.User.get_by_ids user_ids
let get_translations (id, src_lang, dst_lang) =
  Db.Translation.get_translations id src_lang dst_lang
let set_translation (lword, rword, descr, src_lang, dst_lang, user_id) =
  Db.Translation.set lword rword src_lang dst_lang ~description:descr user_id
let delete_translation (id, is_original) =
  Db.Translation.delete id is_original
let update_translation (id, src_lang, dst_lang, lword, rword, descr) =
  Db.Translation.update_translation id src_lang dst_lang lword rword descr
let ask_correction (original_id, corrector_id) =
  Db.Translation.ask_correction original_id corrector_id
let validate_correction correction_id =
  Db.Translation.validate_correction correction_id
let acknowledge_validated_correction original_id correction_id =
  Db.Translation.acknowledge_validated_correction
    ~implement_correction:true original_id correction_id
let cancel_validated_correction original_id correction_id =
  Db.Translation.acknowledge_validated_correction
    ~implement_correction:false
    original_id correction_id

let rpc_update_preferred_lang_src =
  server_function Json.t<int32 * string> update_preferred_lang_src

let rpc_update_preferred_lang_dst =
  server_function Json.t<int32 * string> update_preferred_lang_dst

let rpc_get_supported_lang =
  server_function Json.t<unit> Db.LangDb.get_supported_languages

let rpc_get_translations =
  server_function Json.t<int32 * string * string> get_translations

let rpc_set_translation =
  server_function Json.t<string
                         * string
                         * string
                         * string
                         * string
                         * int32> set_translation

let rpc_delete_translation =
  server_function Json.t<int32
                         * bool> delete_translation
let rpc_update_translation =
  server_function Json.t<int32
                         * string
                         * string
                         * string
                         * string
                         * string> update_translation
let rpc_get_users =
  server_function Json.t<int32 list> get_users

}}

{client{

let update_translations model =
  let arg = Edit_dictionary_model.(get_user_id model,
                                   get_preferred_lang_src model,
                                   get_preferred_lang_dst model) in
  lwt translations = %rpc_get_translations arg in
  let different_ids = Utils.Translation.get_distinct_owners translations in
  lwt owners = %rpc_get_users different_ids in
  let translations = Utils.convert_translations translations owners in
  let trans = List.map (Edit_dictionary_model.to_translation model)
                       translations in
  Lwt.return (Edit_dictionary_model.update_translations model trans)

let update_preferred_lang f model src_or_dst preferred_lang =
  lwt () =
    match src_or_dst with
    | `Src -> %rpc_update_preferred_lang_src
                 Edit_dictionary_model.(get_user_id model,
                                        preferred_lang)
    | `Dst -> %rpc_update_preferred_lang_dst
                 Edit_dictionary_model.(get_user_id model,
                                        preferred_lang)
  in
  let model = Edit_dictionary_model.update_preferred_lang model
                                                          src_or_dst
                                                          preferred_lang in
  lwt new_model = update_translations model in
  let () = f new_model in
  Lwt.return_unit

let change_preferred_lang f model src_or_dst =
  lwt languages = %rpc_get_supported_lang () in
    let () = f (Edit_dictionary_model.change_preferred_lang model
                                                            src_or_dst
                                                            languages) in
  Lwt.return_unit

let back_to_init f model src_or_dst =
  let () = f (Edit_dictionary_model.back_to_init model src_or_dst) in
  Lwt.return_unit

let delete_in_translation_in_db model translation =
  let id, is_original =
    Edit_dictionary_model.Translation.(get_id translation,
                                       is_original translation)
  in
  lwt () = %rpc_delete_translation (id, is_original) in
  Lwt.return (Edit_dictionary_model.delete_translation_from_model
                model translation)

let update_translation f model id source dest description =
  let preferred_lang_src,
      preferred_lang_dst =
    Edit_dictionary_model.(get_preferred_lang_src model,
                           get_preferred_lang_dst model) in
  lwt () = %rpc_update_translation (id,
                                    preferred_lang_src,
                                    preferred_lang_dst,
                                    source,
                                    dest,
                                    description) in
  (* XXX we don't reload the page, i.e.
     call update_translations, to limit the number of db queries *)
  let new_model = Edit_dictionary_model.update_translation_value
                    model
                    id
                    (`EditSrcDstDescription (source, dest, description)) in
  let () = f new_model in
  Lwt.return_unit

let add_translation f model source dest description =
  let user_id,
      preferred_lang_src,
      preferred_lang_dst,
      username
    = Edit_dictionary_model.(get_user_id model,
                             get_preferred_lang_src model,
                             get_preferred_lang_dst model,
                             get_user_username model)
  in
  lwt new_model =
    match_lwt (%rpc_set_translation (source,
                                     dest,
                                     description,
                                     preferred_lang_src,
                                     preferred_lang_dst,
                                     user_id)) with
    | `Ok (id, description) ->
       let owner = Utils.Owner.({username;
                                 preferred_lang_src;
                                 preferred_lang_dst;
                                 id=user_id}) in
       let content = Utils.TranslationInModel.({id;
                                                source;
                                                dest;
                                                description;
                                                owner}) in
       let correction = None in
       let new_translation = Utils.TranslationInModel.({content;
                                                        correction}) in
       Lwt.return (Edit_dictionary_model.add_translation model
                                                         new_translation)
  | `Nok -> Lwt.return (Edit_dictionary_model.set_translation_error model)
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
  let new_model = Edit_dictionary_model.set_translation_as_edit model
                                                                translation in
  let () = f new_model in
  Lwt.return_unit

let cancel_edit_translation f model translation =
  let new_model = Edit_dictionary_model.set_translation_as_read model
                                                                translation in
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
