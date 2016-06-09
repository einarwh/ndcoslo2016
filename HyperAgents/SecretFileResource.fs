module SecretFileResource

open System
open Suave
open Chiron

open Siren
open Utils

type SecretFileLocation = RoomLocation of Uri | TakenByAgent of AgentColor

type Message =
  | LocationQuery of AsyncReplyChannel<SecretFileLocation>
  | Dropped of Uri
  | WebMessage of (HttpContext * AgentColor) * AsyncReplyChannel<WebPart>

let initLoc = getRandomStartLocation()
let initialLocation = initLoc |> toUri

let get ctx clr =  
  let path = "secret-file"
  let href = path |> linkTo |> toUri |> withQueryString (sprintf "agent=%s" clr) |> uri2str
  let takeFileAction = 
    { name = "take-file"
      ``method`` = "POST"
      title = "Take the secret file!"
      href = href
      fields = [] }
  let doc = 
    { properties = 
        { title = "The secret file" 
          description = "This is it! The secret file! Now grab it, get to the airplane, and don't get blown up on the way!" }
      actions = [ takeFileAction ]
      links = [ selfLinkTo path ] }
  doc

printfn "The secret file can be found in %s" (initialLocation |> justPath) 

let agentRef = Agent<Message>.Start (fun inbox ->
  let rec available (loc : Uri) = async {
    let! msg = inbox.Receive()
    match msg with
    | LocationQuery replyChannel ->
      loc |> RoomLocation |> replyChannel.Reply
    | Dropped url ->
      printfn "Cannot drop secret file when it is available!"
    | WebMessage ((ctx, clr), replyChannel) ->
      match ctx.request.``method`` with
      | HttpMethod.GET ->
        let s = get ctx clr |> Json.serialize |> Json.format
        Successful.OK s |> replyChannel.Reply
      | HttpMethod.POST ->
        let! maybeAgent = AgentsResource.agentRef.PostAndAsyncReply(fun ch -> AgentsResource.Lookup(clr, ch))
        match maybeAgent with
        | None ->
          printfn "Error: agent %s not found in registry." clr
        | Some agent ->
          agent.Post(AgentResource.SecretFileNotification)
          let qs = sprintf "agent=%s" clr
          loc |> withQueryString qs |> uri2str |> Redirection.FOUND |> replyChannel.Reply
        printfn "The secret file was picked up by agent %s." clr
        return! taken clr
      | _ ->
        RequestErrors.METHOD_NOT_ALLOWED "no" |> replyChannel.Reply
    return! available loc }

  and taken agent = async {
    let! msg = inbox.Receive()
    match msg with
    | LocationQuery replyChannel ->
      agent |> TakenByAgent |> replyChannel.Reply
    | Dropped url ->
      printfn "Agent %s dropped the secret file in room %s!" agent (url |> justPath)
      return! available url
    | WebMessage ((ctx, clr), replyChannel) ->
      RequestErrors.NOT_FOUND "The file is taken, you can't see it." |> replyChannel.Reply
    return! taken agent
  }

  initialLocation |> available)

