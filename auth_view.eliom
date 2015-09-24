{client{
  open Eliom_content
  open Tyxml_js


  let input_value i = Js.to_string (To_dom.of_input i) ## value
  let create_input ?input_type:(it=`Text) name = Html5.(input ~a:[a_input_type it; a_class ["form-control"]; a_placeholder name] ())

  let create_button ?additional_class:(a=[]) t name onclick =
    let b_class =
      match t with
      | `Action -> ["btn"; "btn-primary"; "btn-block"]
      | `Goto -> ["btn"; "btn-success"; "btn-xs"]
    in
    let b_class = b_class @ a in
    let b = Html5.(button ~a:[a_class b_class] [pcdata name]) in
    let () = Lwt_js_events.(async (fun () -> clicks
                                             (To_dom.of_button b)
                                             (fun _ _ -> onclick ()))) in
    b

  let wrap_in_form_signin content =
    [Html5.(div ~a:[a_class ["form-signin"]] content)]

  let auth_content f create_authenticated_page auth =
    match auth with
    | `Login l -> begin
        let user, password = create_input "Username", create_input ~input_type:`Password "Password" in
        let action_button = create_button `Action "Login"
                                          (fun () ->
                                           let u, p = input_value user, input_value password in
                                           Auth_controller.login f u p) in
        let goto_button = create_button `Goto "Go to create account"
                                         (fun () ->
                                          Auth_controller.goto_create_account f) in

        let res = [user; password; action_button; goto_button] in
        let content = match l with
          | `Unit -> res
          | `Error x -> res @ [Html5.pcdata x]
        in
        wrap_in_form_signin content
      end
    | `CreateAccount ca -> begin
        let user, pass, confirm = create_input "Username", create_input ~input_type:`Password "Password", create_input ~input_type:`Password "Confirm password" in
        let action_button = create_button `Action "Create account"
                                          (fun () ->
                                           let u, p, r = input_value user, input_value pass, input_value confirm in
                                           Auth_controller.create_account f u p r) in
        let goto_button = create_button `Goto "Go to login"
                                         (fun () ->
                                          Auth_controller.goto_login f) in
        let res = [user; pass; confirm; action_button; goto_button] in
        let content = match ca with
        | `Unit -> res
        | `AccountCreated x -> res @ [Html5.pcdata x]
        | `Error (x, e_user) ->
           let () = match e_user with
             | None -> ()
             | Some str ->
                (To_dom.of_input user) ## value <- Js.string str
           in
           res @ [Html5.pcdata x]
        in
        wrap_in_form_signin content
      end
    | `Logged user ->
       let logout = create_button `Goto "Logout" (fun () ->
                                                  Auth_controller.logout f) in

       Html5.([table [tr [td [logout;
                              h2 [pcdata (Printf.sprintf "Ongi etorri %s" user.Current_user.username)]
                             ]
                         ]];
               create_authenticated_page user])

  let view ((r, f): Auth_model.rp) create_auth_page =
    R.Html5.(div (Utils.ReactList.list (React.S.map (auth_content f create_auth_page) r)))

  let setup create_auth_page =
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
    let () = Dom.appendChild parent (Tyxml_js.To_dom.of_div (view rp create_auth_page)) in
    Lwt.return_unit

}}
