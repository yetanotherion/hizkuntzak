(**************************************************************************)
(*  Copyright 2014, Ion Alberdi <nolaridebi at gmail.com>                 *)
(*                                                                        *)
(*  Licensed under the Apache License, Version 2.0 (the "License");       *)
(*  you may not use this file except in compliance with the License.      *)
(*  You may obtain a copy of the License at                               *)
(*                                                                        *)
(*      http://www.apache.org/licenses/LICENSE-2.0                        *)
(*                                                                        *)
(*  Unless required by applicable law or agreed to in writing, software   *)
(*  distributed under the License is distributed on an "AS IS" BASIS,     *)
(*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or       *)
(*  implied.  See the License for the specific language governing         *)
(*  permissions and limitations under the License.                        *)
(**************************************************************************)
{shared{

let conjugate_nor_present param =
  match param with
    | `Ni -> "naiz"
    | `Hi -> "haiz"
    | `Hura -> "da"
    | `Gu -> "gara"
    | `Zu -> "zara"
    | `Zuek -> "zarete"
    | `Haiek -> "dira"

let conjugate_nor_past param =
  match param with
    | `Ni -> "nintzen"
    | `Hi -> "hintzen"
    | `Hura -> "zen"
    | `Gu -> "ginen"
    | `Zu -> "zinen"
    | `Zuek -> "zineten"
    | `Haiek -> "ziren"

let conjugate_nor param time =
  match time with
    | `Present -> conjugate_nor_present param
    | `Past -> conjugate_nor_past param

let conjugate_nor_nork_present param =
  let nor, nork = param in
  let ending =
    match nork with
      | `Nik -> "t"
      | `Hik p ->  begin
        match p with
          | `Male -> "k"
          | `Female -> "n"
      end
      | `Hark -> ""
      | `Guk -> "gu"
      | `Zuk -> "zu"
      | `Zuek -> "zue"
      | `Haiek -> begin
        match nor with
          | `Gu | `Zu | `Haiek -> "zte"
          | `Ni | `Hi | `Hura | `Zuek -> "te"
      end
  in
  let start =
    match nor with
      | `Ni -> "nau"
      | `Hi -> "hau"
      | `Hura -> "du"
      | `Gu -> "gaitu"
      | `Zu -> "zaitu"
      | `Zuek -> "zaituzte"
      | `Haiek -> "ditu"
  in
  start ^ ending

let conjugate_nor_nork_past_others nor nork =
  let ending =
    match nork with
      | `Haiek -> begin
        match nor with
          | `Gu | `Zu -> "zten"
          | `Ni | `Hi | `Zuek -> "ten"
      end
      | `Nik -> "dan"
      | `Hik p -> begin
        match p with
          | `Male -> "an"
          | `Female -> "nan"
      end
      | `Hark -> begin
        match nor with
          | `Zuek -> "n"
          | `Ni | `Hi | `Gu | `Zu -> "en"
      end
      | `Guk -> "gun"
      | `Zuk -> "zun"
      | `Zuek -> "zuen"
  in
  let start =
    match nor with
      | `Ni -> "nindu"
      | `Hi -> "hindu"
      | `Gu -> "gintu"
      | `Zu -> "zintu"
      | `Zuek -> "zintuzte"
  in
  start ^ ending

let conjugate_nor_nork_past_hura nork =
  match nork with
    | `Nik -> "nuen"
    | `Hik _ -> "huen"
    | `Hark -> "zuen"
    | `Zuk -> "zenuen"
    | `Guk -> "genuen"
    | `Zuek -> "zenuten"
    | `Haiek -> "zuten"

let conjugate_nor_nork_past_haiek nork =
  match nork with
    | `Nik -> "nituen"
    | `Hik _ -> "hituen"
    | `Hark -> "zituen"
    | `Zuk -> "zenituen"
    | `Guk -> "genituen"
    | `Zuek -> "zenituzten"
    | `Haiek -> "zituzten"

let create_restricted_nor nor =
  match nor with
    | `Ni -> `Ni
    | `Hi -> `Hi
    | `Gu -> `Gu
    | `Zu -> `Zu
    | `Zuek -> `Zuek
    | `Hura | `Haiek -> assert(false)

let conjugate_nor_nork_past param =
  let nor, nork = param in
  match nor with
    | `Ni | `Hi | `Gu | `Zu | `Zuek -> conjugate_nor_nork_past_others (create_restricted_nor nor) nork
    | `Hura -> conjugate_nor_nork_past_hura nork
    | `Haiek -> conjugate_nor_nork_past_haiek nork

let conjugate_nor_nork p time =
  match time with
    | `Present -> conjugate_nor_nork_present p
    | `Past -> conjugate_nor_nork_past p

let conjugate param time =
  match param with
    | `Nor p -> conjugate_nor p time
    | `NorNork p -> conjugate_nor_nork p time
}}
