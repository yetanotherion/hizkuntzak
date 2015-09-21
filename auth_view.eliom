{client{
  open Eliom_content
  open Tyxml_js

  module ReactList = struct
    let list t =
      let open ReactiveData.RList in
      make_from
        (React.S.value t)
        (React.E.map (fun e -> Set e) (React.S.changes t))
  end


  let input_value i = Js.to_string (To_dom.of_input i) ## value
  let create_input () = Html5.(input ~a:[a_input_type `Text] ())

  let create_button t name onclick =
    let b_class =
      match t with
      | `Action -> ["btn"; "btn-primary"]
      | `Goto -> ["btn"; "btn-warning"]
    in
    let b = Html5.(button ~a:[a_class b_class] [pcdata name]) in
    let () = Lwt_js_events.(async (fun () -> clicks
                                             (To_dom.of_button b)
                                             (fun _ _ -> onclick ()))) in
    b

  let create_user_password_inputs () =
    let user, pass = create_input (), create_input () in
    let t = Html5.(table [
                       tr [td [pcdata "Username:"];
                           td [user]];
                       tr [td [pcdata "Password:"];
                           td [pass]]]) in
    t, user, pass

  let auth_content f auth =
    match auth with
    | `Login l -> begin
        let t, u, p = create_user_password_inputs () in
        let action_button = create_button `Action "Login"
                                          (fun () ->
                                           let u, p = input_value u, input_value p in
                                           Auth_controller.login f u p) in
        let go_to_button = create_button `Goto "Go to create account"
                                         (fun () ->
                                          Auth_controller.goto_create_account f) in
        let res = [t; action_button; go_to_button] in
        match l with
        | `Unit -> res
        | `Error x -> res @ [Html5.pcdata x]
      end
    | `CreateAccount ca -> begin
        let t, u, p = create_user_password_inputs () in
        let action_button = create_button `Action "Create account"
                                          (fun () ->
                                           let u, p = input_value u, input_value p in
                                           Auth_controller.create_account f u p) in
        let go_to_button = create_button `Goto "Go to login"
                                         (fun () ->
                                          Auth_controller.goto_login f) in
        let res = [t; action_button; go_to_button] in
        match ca with
        | `Unit -> res
        | `AccountCreated x | `Error x -> res @ [Html5.pcdata x]
      end
    | `Logged user ->
        let button = create_button `Goto "Logout" (fun () ->
                                                   Auth_controller.logout f) in
        [Html5.pcdata (Printf.sprintf "Ongi etorri %s" user.Current_user.username); button]

  let view ((r, f): Auth_model.rp) =
    R.Html5.(div (ReactList.list (React.S.map (auth_content f) r)))

  let setup () =
    let doc = Dom_html.document in
    let parent =
      Js.Opt.get (doc##getElementById(Js.string "main"))
        (fun () -> assert false)
    in
    lwt model =
      match_lwt Auth_controller.get_current_user () with
       | None -> Lwt.return Auth_model.not_logged
       | Some u -> Lwt.return (Auth_model.logged u)
    in
    let rp = React.S.create model in
    let () = Dom.appendChild parent (Tyxml_js.To_dom.of_div (view rp)) in
    Lwt.return_unit

}}
