{client{

type login = [ `Unit | `Error of string]
type create_account = [ `Unit
                      | `AccountCreated of string
                      | `Error of string]

type state = [ `Login of login
             | `CreateAccount of create_account
             | `Logged of Current_user.t]

let not_logged = `Login `Unit
let logged u = `Logged u

type rs = state React.signal
type rf = ?step:React.step -> state -> unit
type rp = rs * rf

}}
