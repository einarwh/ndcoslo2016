module StartResource

open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful

open Chiron
open Utils
open Siren

type Agent<'T> = MailboxProcessor<'T>
type RequestInfo = HttpContext
type ResponseInfo =
      | Success of SirenDocument
      | Failure

type Message = RequestInfo * AsyncReplyChannel<WebPart>

type StartPlayerResult = StartedWhite of string | StartedBlack of string | FailedToStart

let getStartPlayerActions includeBlack includeWhite =
  let agentField = { name = "agent"; ``type`` = "text"; value = None }

  let startAgentAction = 
    { name = "start-agent"
      ``method`` = "POST"
      title = "Start agent"
      href = linkTo "start"
      fields = [ agentField ] }

  let result = [ startAgentAction ]
  result |> List.filter (fun a -> includeBlack || includeWhite)

let get ctx includeBlack includeWhite =  
  System.Console.WriteLine("get /start")
  let doc = 
    { properties = { title = "Agent vs Agent"; description = "Welcome to the HyperAgents game!" }
      actions = getStartPlayerActions includeBlack includeWhite
      links = [ selfLinkTo "start" ] }
  doc

let r = new System.Random()

let getRandomStartLocation =
  let locations = [ "teleporter-room"; "exit-room"; "files-room"; "boring-room" ]
  let roomIndex = r.Next(List.length locations)
  locations.Item roomIndex |> linkTo

let start agent =
  match agent with 
  | "black" -> StartedBlack getRandomStartLocation
  | "white" -> StartedWhite getRandomStartLocation
  | _ -> FailedToStart

let startPlayer ctx =
  System.Console.WriteLine ("post /start")
  System.Console.WriteLine (ctx.request.form)
  match ctx.request.formData "agent" with
  | Choice1Of2 agent -> start agent
  | Choice2Of2 x -> FailedToStart

let agentRef = Agent<Message>.Start (fun inbox ->
  let rec none() = async {
    let! msg = inbox.Receive()
    let (ctx, replyChannel) = msg

    let (state, webPart) =
      match ctx.request.``method`` with
      | HttpMethod.GET -> 
        let s = get ctx true true |> Json.serialize |> Json.format
        (none, Successful.OK s)
      | HttpMethod.POST ->
        match startPlayer ctx with
        | StartedBlack loc ->
          (black, Redirection.FOUND loc)
        | StartedWhite loc ->
          (* Redirect to random start location. *)
          (white, Redirection.FOUND loc)
        | FailedToStart ->
          (none, RequestErrors.BAD_REQUEST "no")
      | _ -> 
        (none, RequestErrors.METHOD_NOT_ALLOWED "no")
    webPart |> replyChannel.Reply
    return! state() }

  and black() = async {
    let! msg = inbox.Receive()
    let (ctx, replyChannel) = msg

    let (state, webPart) =
      match ctx.request.``method`` with
      | HttpMethod.GET -> 
        let s = get ctx false true |> Json.serialize |> Json.format
        (none, Successful.OK s)
      | HttpMethod.POST ->
        match startPlayer ctx with
        | StartedBlack loc ->
          (* Logic error! *)
          (black, RequestErrors.CONFLICT "no")
        | StartedWhite loc ->
          (* Redirect to random start location. *)
          (both, Redirection.FOUND loc)
        | FailedToStart ->
          (black, RequestErrors.BAD_REQUEST "no")
      | _ -> 
        (none, RequestErrors.METHOD_NOT_ALLOWED "no")
    webPart |> replyChannel.Reply
    return! state() }

  and white() = async {
    let! msg = inbox.Receive()
    let (ctx, replyChannel) = msg

    let (state, webPart) =
      match ctx.request.``method`` with
      | HttpMethod.GET -> 
        let s = get ctx true false |> Json.serialize |> Json.format
        (none, Successful.OK s)
      | HttpMethod.POST ->
        match startPlayer ctx with
        | StartedBlack loc ->
          (both, Redirection.FOUND loc)
        | StartedWhite loc ->
          (* Logic error! *)
          (white, RequestErrors.CONFLICT "no")
        | FailedToStart ->
          (none, RequestErrors.BAD_REQUEST "no")
      | _ -> 
        (none, RequestErrors.METHOD_NOT_ALLOWED "no")
    webPart |> replyChannel.Reply
    return! state() }

  and both() = async {
    let! msg = inbox.Receive()
    let (ctx, replyChannel) = msg

    let (state, webPart) =
      match ctx.request.``method`` with
      | HttpMethod.GET -> 
        let s = get ctx false false |> Json.serialize |> Json.format
        (none, Successful.OK s)
      | _ -> 
        (none, RequestErrors.METHOD_NOT_ALLOWED "no")
    webPart |> replyChannel.Reply
    return! state() }

  none()
)
