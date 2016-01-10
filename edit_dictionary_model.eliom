{client{
  type preferred_lang_state = [`Unit | `Change_preferred_lang of string list]
  type add_translation_state = [`Ok | `Duplicated_translation]
  type global_state = [`Learn | `Play]
  type state = {src_preferred_lang_state: preferred_lang_state;
                dst_preferred_lang_state: preferred_lang_state;
                add_translation_state: add_translation_state }


  module Translation = struct
      type state = [`Read | `Edit ]
      type t = {
          state: state;
          value: Utils.TranslationInModel.t;
        }
    end
  type t = {
      current_user: Current_user.t;
      state: state;
      translations: Translation.t list;
      global_state: global_state;
    }

  type rs = t React.signal
  type rf = ?step:React.step -> t -> unit
  type rp = rs * rf

  let create current_user = {current_user; state={src_preferred_lang_state=`Unit;
                                                  dst_preferred_lang_state=`Unit;
                                                  add_translation_state=`Ok};
                             translations=[]; global_state=`Learn}

  let get_user_id t = Current_user.(get_user_id t.current_user)
  let get_preferred_lang_src t = Current_user.(get_preferred_lang_src t.current_user)
  let get_preferred_lang_dst t = Current_user.(get_preferred_lang_dst t.current_user)
  let get_user_username t = Current_user.(get_username t.current_user)
  let get_translations t =
    List.sort (fun x y ->
               let getval arg = Translation.(arg.value) in
               let xval, yval = getval x, getval y in
               Pervasives.compare xval yval) t.translations

  let get_translation translation = Translation.(translation.value)

  let update_preferred_lang t src_or_dst lang =
    let new_user, new_state =
      match src_or_dst with
      | `Src -> {t.current_user with Current_user.preferred_lang_src = lang},
                {t.state with src_preferred_lang_state = `Unit}
      | `Dst -> {t.current_user with Current_user.preferred_lang_dst = lang},
                {t.state with dst_preferred_lang_state = `Unit}
    in
    {t with current_user = new_user;
            state = new_state}

  let change_preferred_lang t src_or_dst newlanguages =
    let new_lang_state = `Change_preferred_lang newlanguages in
    let new_state =
      match src_or_dst with
      | `Src -> {t.state with src_preferred_lang_state = new_lang_state}
      | `Dst -> {t.state with dst_preferred_lang_state = new_lang_state}
    in
    {t with state = new_state}

  let back_to_init t src_or_dst =
    let new_state =
      match src_or_dst with
      | `Src -> {t.state with src_preferred_lang_state = `Unit}
      | `Dst -> {t.state with dst_preferred_lang_state = `Unit}
    in
    {t with state = new_state}

  let update_translations t translations =
    {t with translations = translations}

  let set_translation_as_read t translation =
    {translation with Translation.state = `Read}

  let create_translation translation =
    Translation.({state=`Read; value=translation})

  let delete_translation t translation =
    List.filter (fun x ->
                 Translation.(x.value <> translation.value)) t.translations

  let add_translation t translation =
    update_translations t (translation :: t.translations)

  let set_translation_error t =
    let new_state = {t.state with
                      add_translation_state = `Duplicated_translation} in
    {t with state = new_state}

  let set_translation_ok t =
    let new_state = {t.state with add_translation_state = `Ok} in
    {t with state = new_state}

  let update_translation_state state t translation =
    let new_translation = Translation.({translation with state = state}) in
    update_translations t
                        (new_translation :: (delete_translation t translation))

  let set_translation_as_edit = update_translation_state `Edit
  let set_translation_as_read = update_translation_state `Read

  let update_translation_value t id operation =
    (* don't look for corrections yet *)
    let element, others = List.partition (fun x ->
                                          let value = x.Translation.value in
                                          Utils.TranslationInModel.(
                                            value.content.id )= id)
                                         t.translations in
    match element with
    | [] -> t
    | hd :: _ ->
       let open Utils.TranslationInModel in
       let content = hd.Translation.value.content in
       let new_content =
         match operation with
         | `EditSrcDstDescription (source, dest, description) ->
            {content with source=source; dest=dest; description=description} in
       let new_value = {hd.Translation.value with content = new_content} in
       let new_translation = Translation.({value = new_value;
                                           state = `Read}) in
       update_translations t (new_translation :: others)


  let delete_translation_from_model model translation =
    update_translations model (delete_translation model translation)

  let back_to_learning model =
    {model with global_state = `Learn }

  let goto_play model =
    {model with global_state = `Play }

}}
