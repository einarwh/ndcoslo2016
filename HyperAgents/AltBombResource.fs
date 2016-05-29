module AltBombResource

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

let attemptDisarm (ctx : HttpContext) : ResponseInfo =  

  let fieldData = 
    match (ctx.request.formData "name", ctx.request.formData "type", ctx.request.formData "value") with
    | Choice1Of2 name, Choice1Of2 "text", Choice1Of2 value -> Some (name, value)
    | _ -> None

  match fieldData with
  | None ->
    let selfLink = { rel = [ "self" ]; href = "http://bomb" }

    let doc = 
      { properties = { title = "Bomb"; description = "Bad request, tbh" }
        actions = []
        links = [ selfLink ] }
    Failure  

  | Some (wire, "red") ->
    let selfLink = { rel = [ "self" ]; href = "http://bomb" }

    let doc = 
      { properties = { title = "Bomb"; description = "Successfully Disarmed!" }
        actions = []
        links = [ selfLink ] }
    Success doc  

  | Some (wire, _) ->
    let selfLink = { rel = [ "self" ]; href = "http://bomb" }

    let doc = 
      { properties = { title = "Bomb"; description = "BOOM!" }
        actions = []
        links = [ selfLink ] }
    Success doc  

let getArmed ctx = 
  let cutWireAction = 
    { name = "cut-red-wire"
      ``method`` = "POST"
      title = "Cut wire"
      href = "http://bomb"
      fields = [] }
  let cutRedWireAction =
    { cutWireAction with name = "cut-red-wire"; title = "Cut the red wire"; fields = [ { name = "wire"; ``type`` = "text"; value = Some (String "red") } ]} 
  let cutBlueWireAction =
    { cutWireAction with name = "cut-blue-wire"; title = "Cut the blue wire"; fields = [ { name = "wire"; ``type`` = "text"; value = Some (String "blue") } ]} 
  let doc = 
    { properties = 
        { title = "Bomb"
          description = "You have encountered a Die Hard-style scary bomb. There is some sort of liquid flowing in a container. You see a red and a blue wire." }
      actions = [ cutRedWireAction; cutBlueWireAction ]
      links = [] }
  doc

let getDisarmed ctx = 
  let doc = 
    { properties = 
        { title = "Bomb"
          description = "The bomb has been disarmed. Yay you." }
      actions = []
      links = [] }
  doc

let agentRef = Agent<Message>.Start (fun inbox ->

  let rec armed() = async {
    let! msg = inbox.Receive()
    let (ctx, replyChannel) = msg  

    let (state, webPart) = 
      match ctx.request.``method`` with
      | HttpMethod.GET -> 
        let s = getArmed ctx |> Json.serialize |> Json.format
        (armed, Successful.OK s)
      | HttpMethod.POST ->
        match attemptDisarm ctx with
        | Success (x : SirenDocument) ->
          let s = x |> Json.serialize |> Json.format
          (disarmed, Successful.OK s)
        | Failure ->
          (armed, RequestErrors.BAD_REQUEST "no")
      | _ -> 
        (armed, RequestErrors.METHOD_NOT_ALLOWED "no")
    webPart |> replyChannel.Reply
    return! state()        
    }

  and disarmed() = async {
    let! msg = inbox.Receive()
    let (ctx, replyChannel) = msg  

    let (state, webPart) = 
      match ctx.request.``method`` with
      | HttpMethod.GET -> 
        let s = getDisarmed ctx |> Json.serialize |> Json.format
        (disarmed, Successful.OK s)
      | _ -> 
        (disarmed, RequestErrors.METHOD_NOT_ALLOWED "no")
    webPart |> replyChannel.Reply
    return! state()        
  }

  armed()
)
