module AltBombResource

open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful

open Chiron
open Siren

type Agent<'T> = MailboxProcessor<'T>
type RequestInfo = HttpContext
type DisarmResult = Disarmed of SirenDocument | Explosion of SirenDocument
type ResponseInfo =
      | Valid of DisarmResult
      | Invalid

type Message = RequestInfo * AsyncReplyChannel<WebPart>

let cutWire ctx wireColor : DisarmResult =
  match wireColor with
  | "red" ->
    Disarmed 
      { properties = { title = "Bomb"; description = "Successfully Disarmed!" }
        actions = []
        links = [ selfLinkTo "bomb" ] }
  | _ ->
    Explosion 
      { properties = { title = "Bomb"; description = "BOOM!" }
        actions = []
        links = [ selfLinkTo "bomb" ] }

let attemptDisarm (ctx : HttpContext) : ResponseInfo =  

  let wireColor = 
    match (ctx.request.formData "name", ctx.request.formData "type", ctx.request.formData "value") with
    | Choice1Of2 name, Choice1Of2 "text", Choice1Of2 value -> Some value
    | _ -> None

  match wireColor with
  | None ->
    Invalid  
  | Some color -> cutWire ctx color |> Valid

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

let getExploded ctx = 
  let doc = 
    { properties = 
        { title = "Bomb"
          description = "The bomb has exploded. And I don't know why I'm even telling you this, because you are dead." }
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
        | Valid (res : DisarmResult) ->
          BoobyTrappedRoomResource.agentRef.Post(BoobyTrappedRoomResource.DisarmNotification)
          match res with
          | Disarmed d ->
            let s = d |> Json.serialize |> Json.format
            (disarmed, Successful.OK s)
          | Explosion e ->
            let s = e |> Json.serialize |> Json.format
            (exploded, Successful.OK s)
        | Invalid ->
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

  and exploded() = async {
    let! msg = inbox.Receive()
    let (ctx, replyChannel) = msg  

    let (state, webPart) = 
      match ctx.request.``method`` with
      | HttpMethod.GET -> 
        let s = getExploded ctx |> Json.serialize |> Json.format
        (disarmed, Successful.OK s)
      | _ -> 
        (disarmed, RequestErrors.METHOD_NOT_ALLOWED "no")
    webPart |> replyChannel.Reply
    return! state()          
  }

  armed()
)
