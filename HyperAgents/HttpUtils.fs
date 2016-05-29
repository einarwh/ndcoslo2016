module HttpUtils

open Siren

type ResponseBodyType = Siren of SirenDocument | Text of string | Empty
