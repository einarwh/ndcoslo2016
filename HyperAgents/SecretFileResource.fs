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
  printfn "get /secret-file"
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

let agentRef = Agent<Message>.Start (fun inbox ->
  let rec available loc = async {
    printfn "Secret file is available and awaiting messages. Initial location: %s" initLoc
    let! msg = inbox.Receive()
    match msg with
    | LocationQuery replyChannel ->
      printfn "Where are you, secret file?"
      loc |> RoomLocation |> replyChannel.Reply
    | Dropped url ->
      printfn "Logical error: cannot drop secret file when it is available!"
    | WebMessage ((ctx, clr), replyChannel) ->
      printfn "Represent secret file."
      match ctx.request.``method`` with
      | HttpMethod.GET ->
        let s = get ctx clr |> Json.serialize |> Json.format
        Successful.OK s |> replyChannel.Reply
      | HttpMethod.POST ->
        printfn "Agent %s wants to pick up the secret file!" clr
        (* Agent should know they got the secret file. *)
        let! maybeAgent = AgentsResource.agentRef.PostAndAsyncReply(fun ch -> AgentsResource.Lookup(clr, ch))
        match maybeAgent with
        | None ->
          printfn "Error: agent %s not found in registry." clr
        | Some agent ->
          agent.Post(AgentResource.SecretFileNotification)
          let qs = sprintf "agent=%s" clr
          loc |> withQueryString qs |> uri2str |> Redirection.FOUND |> replyChannel.Reply
        return! taken clr
      | _ ->
        RequestErrors.METHOD_NOT_ALLOWED "no" |> replyChannel.Reply
    return! available loc }

  and taken agent = async {
    printfn "Secret file is taken and awaiting messages. Initial location: %s" initLoc
    let! msg = inbox.Receive()
    match msg with
    | LocationQuery replyChannel ->
      printfn "Where are you, secret file?"
      agent |> TakenByAgent |> replyChannel.Reply
    | Dropped url ->
      printfn "Dropped the file at %s!" <| url.ToString()
      return! available url
    | WebMessage ((ctx, clr), replyChannel) ->
      RequestErrors.NOT_FOUND "It's taken, you can't see it." |> replyChannel.Reply
    return! taken agent
  }

  initialLocation |> available)

