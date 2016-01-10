{client{
  type preferred_lang_state = [`Unit | `Change_preferred_lang of string list]
  type add_translation_state = [`Ok | `Duplicated_translation]
  type global_state = [`Learn | `Play]
  type state = {src_preferred_lang_state: preferred_lang_state;
                dst_preferred_lang_state: preferred_lang_state;
                add_translation_state: add_translation_state }


  module Translation = struct
      type state = [`Read | `Edit ]

      module Correction = struct
          type t = {
              data: Utils.TranslationInModel.data;
              original: Utils.TranslationInModel.data;
              state: state;
            }
        end

      module Original = struct
          type data = Utils.TranslationInModel.data
          type correction_state = [`None
                                  | `ChoosingCorrector
                                  | `CorrectorDoesNotExist of string
                                  | `CorrectorChosen of data
                                  | `CorrectionDone of data ]
          type t = {
              data: Utils.TranslationInModel.data;
              correction_state: correction_state;
              state: state;
            }
        end

      type t = [`Original of Original.t
               | `Correction of Correction.t]

      let set_state translation state =
        match translation with
        | `Original x -> `Original {x with
                                     Original.state = state}
        | `Correction x -> `Correction {x with
                                         Correction.state = state}

      let get_id t =
        match t with
        | `Original x -> Utils.TranslationInModel.get_data_id
                           x.Original.data
        | `Correction x -> Utils.TranslationInModel.get_data_id
                             x.Correction.data
      let is_original t =
        match t with
        | `Original _ -> true
        | `Correction _ -> false

      let get_compare_key t =
        let get_username data =
          Utils.(
                Owner.get_username
                  data.TranslationInModel.owner)
        in
        match t with
        | `Original x ->
           Original.(
            let username = get_username x.data in
            ("b", username, x.data.Utils.TranslationInModel.source))
        | `Correction x ->
           Correction.(
            let username = get_username x.original in
            ("a", username, x.data.Utils.TranslationInModel.source))
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
               let getval = Translation.get_compare_key in
               let xval, yval = getval x, getval y in
               Pervasives.compare xval yval) t.translations

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

  let to_translation t translation =
    let user_id = get_user_id t in
    let open Utils.TranslationInModel in
    let get_owner_id x = x.owner.Utils.Owner.id in
    if user_id = get_owner_id translation.content then
      let data = translation.content in
      let correction_state =
        match translation.correction with
        | None -> `None
        | Some x ->
           let correction_data = x.correction_d in
           if x.validated then `CorrectionDone correction_data
           else `CorrectorChosen correction_data
      in
      Translation.Original.(`Original {data;correction_state;state=`Read})
    else
      match translation.correction with
      | None -> failwith("Missing correction")
      | Some x ->
         if (get_owner_id x.correction_d) <> user_id then
           failwith("Correction does not have right id")
         else
           let data = x.correction_d in
           let original = translation.content in
           Translation.Correction.(`Correction {data; original;state=`Read})

  let update_translations t translations =
    {t with translations = translations}

  let delete_translation t translation =
    List.filter (fun x ->
                 Translation.(get_id x <> get_id translation)) t.translations

  let add_translation t translation =
    update_translations t ((to_translation t translation) :: t.translations)

  let set_translation_error t =
    let new_state = {t.state with
                      add_translation_state = `Duplicated_translation} in
    {t with state = new_state}

  let set_translation_ok t =
    let new_state = {t.state with add_translation_state = `Ok} in
    {t with state = new_state}

  let update_translation_state state t translation =
    let new_translation = Translation.set_state translation state  in
    update_translations t
                        (new_translation :: (delete_translation t translation))
  let get_pairs t =
    List.fold_left (fun accum x ->
                    match x with
                    | `Correction _ -> accum
                    | `Original o ->
                       let data = Translation.Original.(o.data) in
                       Utils.TranslationInModel.(
                        data.source,
                        data.dest) :: accum) [] t.translations

  let set_translation_as_edit t translation =
    update_translation_state `Edit t translation
  let set_translation_as_read t translation =
    update_translation_state `Read t translation

  let update_translation_value t id operation =
    let element, others = List.partition (fun x ->
                                          (Translation.get_id x) = id)
                                         t.translations in
    match element with
    | [] -> t
    | hd :: _ ->
       let new_translation =
       match operation with
        | `EditSrcDstDescription (source, dest, description) ->
           let open Translation in
           let open Utils.TranslationInModel in
           match hd with
           | `Original x -> Original.(
               let new_data =
                 {x.data with source=source;
                              dest=dest;
                              description=description} in
               `Original {x with data=new_data; state=`Read})
           | `Correction x -> Correction.(
               let new_data =
                 {x.data with
                   source=source;
                   dest=dest;
                   description=description} in
               `Correction {x with data=new_data; state=`Read})
           in
       update_translations t (new_translation :: others)


  let delete_translation_from_model model translation =
    update_translations model (delete_translation model translation)

  let back_to_learning model =
    {model with global_state = `Learn }

  let goto_play model =
    {model with global_state = `Play }

}}
