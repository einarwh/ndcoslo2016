module StartRoomResource

open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful

open Chiron
open Siren

type Agent<'T> = MailboxProcessor<'T>
type RequestInfo = HttpContext
type ResponseInfo =
      | Success of SirenDocument
      | Failure

type Message = RequestInfo * AsyncReplyChannel<WebPart>

let get ctx =  
  let nameField = { name = "name"; ``type`` = "text"; value = None }
  let classField = { name = "class"; ``type`` = "text"; value = None }
  let raceField = { name = "race"; ``type`` = "text"; value = None }

  let createAction = 
    { name = "start-adventure"
      ``method`` = "POST"
      title = "Start adventure"
      href = "http://void"
      fields = [nameField; classField; raceField] }

  let selfLink = { rel = [ "self" ]; href = "http://void" }

  let doc = 
    { properties = { title = "Void"; description = "The Magical Void" }
      actions = [ createAction ]
      links = [ selfLink ] }
  
  doc

let agentRef = Agent<Message>.Start (fun inbox ->
  let rec loop() = async {
    let! msg = inbox.Receive()
    let (ctx, replyChannel) = msg

    let webPart = 
      match ctx.request.``method`` with
      | HttpMethod.GET -> 
        let s = get ctx |> Json.serialize |> Json.format
        Successful.OK s
      | _ -> 
        RequestErrors.METHOD_NOT_ALLOWED "no"

    webPart |> replyChannel.Reply
    return! loop()
  }
  loop()
)
