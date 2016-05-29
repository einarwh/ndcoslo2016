module BombResource

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

let armed (ctx : HttpContext) =  

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
    doc  

  | Some (wire, "red") ->
    let selfLink = { rel = [ "self" ]; href = "http://bomb" }

    let doc = 
      { properties = { title = "Bomb"; description = "Successfully Disarmed!" }
        actions = []
        links = [ selfLink ] }
    doc  

  | Some (wire, _) ->
    let selfLink = { rel = [ "self" ]; href = "http://bomb" }

    let doc = 
      { properties = { title = "Bomb"; description = "BOOM!" }
        actions = []
        links = [ selfLink ] }
    doc  

let disarmed (ctx : HttpContext) =
  armed ctx

let post ctx = 
  armed ctx

let get ctx = 
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

let agentRef = Agent<Message>.Start (fun inbox ->

  let rec loop oldState = async {
    let! msg = inbox.Receive()
    let (ctx, replyChannel) = msg

    let m = ctx.request.``method``
    let f = 
      match m with 
      | HttpMethod.GET -> "get"
      | HttpMethod.POST -> "post"
      | _ -> "other"

    System.Console.WriteLine(f)

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

    let webPart =
      match m with 
      | HttpMethod.GET -> 
        let s = get ctx |> Json.serialize |> Json.format
        Successful.OK s
      | HttpMethod.POST -> 
        let s = post ctx |> Json.serialize |> Json.format
        Successful.OK s
      | _ -> OK "huh"

    webPart |> replyChannel.Reply
    return! loop oldState
  }
  loop "state"
)
