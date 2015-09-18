{shared{

type t = {
    username: string;
    password: string;
    id: Int32.t;
  }
let create username password id = {
    username; password; id
  }
}}
