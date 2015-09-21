{client{

type login = [ `Unit | `Error of string]
type keep_create_account_form = string
type create_account_error = string * keep_create_account_form option
type create_account = [ `Unit
                      | `AccountCreated of string
                      | `Error of create_account_error]

type state = [ `Login of login
             | `CreateAccount of create_account
             | `Logged of Current_user.t]

let not_logged = `Login `Unit
let logged u = `Logged u

type rs = state React.signal
type rf = ?step:React.step -> state -> unit
type rp = rs * rf

}}
