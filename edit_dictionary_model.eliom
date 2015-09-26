{client{

  type state = [`Unit | `Change_preferred_lang of string list ]
  type t = {
      current_user: Current_user.t;
      state: state;
      translations: Utils.Translation.t list;
    }

  type rs = t React.signal
  type rf = ?step:React.step -> t -> unit
  type rp = rs * rf

  let create current_user = {current_user; state=`Unit; translations=[]}

  let get_user_id t = Current_user.(get_user_id t.current_user)
  let get_preferred_lang t = Current_user.(get_preferred_lang t.current_user)
  let get_translations t = t.translations

  let update_preferred_lang t lang =
    let new_user = {t.current_user with Current_user.preferred_lang = lang} in
    {t with current_user = new_user;
            state = `Unit}

  let change_preferred_lang t newlanguages =
    {t with state = `Change_preferred_lang newlanguages}

  let back_to_init t =
    {t with state = `Unit}

  let update_translations t translations =
    {t with translations = translations}
}}
