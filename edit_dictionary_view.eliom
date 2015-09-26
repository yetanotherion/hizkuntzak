{client{
  open Tyxml_js

  type t = {
      current_user: Current_user.t;
    }

  type rs = t React.signal
  type rf = ?step:React.step -> t -> unit
  type rp = rs * rf

  let create_model current_user = {current_user}

  let view_content _ value =
    Html5.([pcdata (Printf.sprintf "Preferred language %s" value.current_user.Current_user.preferred_lang)])

  let view ((r, f): rp) =
    R.Html5.(div (Utils.ReactList.list (React.S.map (view_content f) r)))

  let create_auth_page user =
    let model = create_model user in
    let rp = React.S.create model in
    view rp

}}
