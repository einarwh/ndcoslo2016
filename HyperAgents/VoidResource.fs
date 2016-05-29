module VoidResource

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
type State = Foo | Bar | Quux

type Message = RequestInfo * AsyncReplyChannel<WebPart>

let updateState state msg = 
  match state with
  | Foo -> Bar
  | Bar -> Quux
  | Quux -> Foo

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

    let state = updateState oldState ctx

    let doc = 
      { properties = 
          { title = "Void"
            description = "The Magical Void" }
        actions = []
        links = [] }

    let webPart =
      match m with 
      | HttpMethod.GET -> 
        let s = get ctx |> Json.serialize |> Json.format
        Successful.OK s
      | HttpMethod.POST -> 
        let s = doc |> Json.serialize |> Json.format
        Successful.CREATED s >=> Writers.addHeader "location" "http://localhost/whee"
      | _ -> OK "huh"

    webPart |> replyChannel.Reply
    return! loop state
  }
  loop Foo
)
