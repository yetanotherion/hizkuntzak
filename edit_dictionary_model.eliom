{client{
  type preferred_lang_state = [`Unit | `Change_preferred_lang of string list]
  type add_translation_state = [`Ok | `Duplicated_translation]
  type state = {preferred_lang_state: preferred_lang_state;
                add_translation_state: add_translation_state}
  module Translation = struct
      type state = [`Read | `Edit]
      type t = {
          state: state;
          value: Utils.Translation.t;
        }
    end
  type t = {
      current_user: Current_user.t;
      state: state;
      translations: Translation.t list;
    }

  type rs = t React.signal
  type rf = ?step:React.step -> t -> unit
  type rp = rs * rf

  let create current_user = {current_user; state={preferred_lang_state=`Unit;
                                                  add_translation_state=`Ok};
                             translations=[]}

  let get_user_id t = Current_user.(get_user_id t.current_user)
  let get_preferred_lang t = Current_user.(get_preferred_lang t.current_user)

  let get_translations t =
    List.sort (fun x y ->
               let getval arg = Translation.(arg.value) in
               let xval, yval = getval x, getval y in
               Pervasives.compare xval yval) t.translations

  let get_translation translation = Translation.(translation.value)

  let update_preferred_lang t lang =
    let new_user = {t.current_user with Current_user.preferred_lang = lang} in
    let new_state = {t.state with preferred_lang_state = `Unit} in
    {t with current_user = new_user;
            state = new_state}

  let change_preferred_lang t newlanguages =
    let new_state = {t.state with preferred_lang_state = `Change_preferred_lang newlanguages} in
    {t with state = new_state}

  let back_to_init t =
    let new_state = {t.state with preferred_lang_state = `Unit} in
    {t with state = new_state}

  let update_translations t translations =
    {t with translations = translations}

  let set_translation_as_read t translation =
    {translation with Translation.state = `Read}


  let create_translation translation =
    Translation.({state=`Read; value=translation})

  let delete_translation t translation =
    List.filter (fun x -> Translation.(x.value <> translation.value)) t.translations

  let add_translation t translation =
    update_translations t (translation :: t.translations)

  let set_translation_error t =
    let new_state = {t.state with add_translation_state = `Duplicated_translation} in
    {t with state = new_state}

  let set_translation_ok t =
    let new_state = {t.state with add_translation_state = `Ok} in
    {t with state = new_state}

  let update_translation_state state t translation =
    let new_translation = Translation.({translation with state = state}) in
    update_translations t (new_translation :: (delete_translation t translation))

  let set_translation_as_edit = update_translation_state `Edit
  let set_translation_as_read = update_translation_state `Read

  let delete_translation_from_model model translation =
    update_translations model (delete_translation model translation)


}}
