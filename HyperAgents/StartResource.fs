module StartResource

open System
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
type Color = string

type StartPlayerResult = Started of Uri * Color | FailedToStart

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

let getRandomStartLocation : string =
  let locations = [ "control-room"; "office"; "laboratory"; "teleport-room"; "exit-room" ]
  let roomIndex = r.Next(List.length locations)
  locations.Item roomIndex |> linkTo

let start (agent : Color) =
  let qs = sprintf "agent=%s" agent
  let loc = getRandomStartLocation |> toUri |> withQueryString qs
  Started (loc, agent)

let startPlayer ctx =
  System.Console.WriteLine ("post /start")
  System.Console.WriteLine (ctx.request.form)
  match ctx.request.formData "agent" with
  | Choice1Of2 agent -> start agent
  | Choice2Of2 x -> FailedToStart

let agentRef = Agent<Message>.Start (fun inbox ->
  let rec start() = async {
    let! msg = inbox.Receive()
    let (ctx, replyChannel) = msg

    let (state, webPart) =
      match ctx.request.``method`` with
      | HttpMethod.GET -> 
        let s = get ctx true true |> Json.serialize |> Json.format
        (start, Successful.OK s)
      | HttpMethod.POST ->
        match startPlayer ctx with
        | Started (loc, color) ->
          AgentsResource.agentRef.Post(AgentsResource.Register(color, AgentResource.createAgent color loc))
          (start, Redirection.FOUND <| loc.ToString())
        | FailedToStart ->
          (start, RequestErrors.BAD_REQUEST "no")
      | _ -> 
        (start, RequestErrors.METHOD_NOT_ALLOWED "no")
    webPart |> replyChannel.Reply
    return! state() }

  start()
)
