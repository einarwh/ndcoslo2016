module AgentResource

open System

open Chiron

open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful

open Siren
open Utils

type Message =
  | WebMessage of (HttpContext * AgentColor) * AsyncReplyChannel<WebPart>
  | LocationUpdate of Uri
  | LocationQuery of AsyncReplyChannel<Uri>
  | SecretFileNotification
  | ExplodingBombNotification

let selfDescription resourceAgentColor path hasSecretFile =
  let desc = sprintf "You are the brillant and sly %s agent. You are currently here: %s." resourceAgentColor path
  if hasSecretFile then
    desc + " You have the secret file, because you are a clever genius!"
  else
    desc

let otherDescription resourceAgentColor =
  sprintf "It is the cowardly and deceitful %s agent, up to no good as usual." resourceAgentColor

let getAlive 
  (resourceAgentColor : string) 
  (maybeReferrer : SirenHref option) 
  (location : Uri) 
  (requestingAgentColor : string)
  (hasSecretFile : bool) =
  let path = location.AbsolutePath.Substring(1)
  let links = 
    match maybeReferrer with
    | None -> []
    | Some ref -> [ { rel = ["back"]; href = ref } ]
  let desc = 
    if requestingAgentColor = resourceAgentColor then 
      selfDescription resourceAgentColor path hasSecretFile
    else
      otherDescription resourceAgentColor
  let doc = 
    { properties = 
        { title = sprintf "The %s agent" resourceAgentColor
          description = desc }
      actions = []
      links = links }
  doc

let createAgent (resourceAgentColor : string) (location : Uri) = 
  Agent<Message>.Start (fun inbox ->
  let rec alive (location : Uri) = async {
    let! msg = inbox.Receive()
    match msg with
    | LocationUpdate loc ->
      printfn "Location update for agent %s: %s" resourceAgentColor (loc |> justPath)
      return! alive loc
    | LocationQuery replyChannel ->
      location |> replyChannel.Reply
      return! alive location
    | SecretFileNotification ->
      printfn "Agent %s notified that they have the secret file!" resourceAgentColor
      return! files location
    | ExplodingBombNotification ->
      printfn "Uh oh agent %s just got a notification that a bomb exploded." resourceAgentColor
      return! dead location
    | WebMessage ((ctx, requestingAgentColor), replyChannel) ->
      let webPart = 
        match ctx.request.``method`` with
        | HttpMethod.GET ->
          let referrer = 
            match ctx.request.header "referer" with 
            | Choice1Of2 url -> Some url
            | Choice2Of2 _ -> None
          let s = getAlive resourceAgentColor referrer location requestingAgentColor false |> Json.serialize |> Json.format 
          Successful.OK s
        | _ ->
          RequestErrors.METHOD_NOT_ALLOWED "no"
      webPart |> replyChannel.Reply
      return! alive location }
  
  and files location = async {
    let! msg = inbox.Receive()
    match msg with
    | LocationUpdate loc ->
      printfn "Location update for agent %s: %s" resourceAgentColor (loc |> justPath)
      return! files loc
    | LocationQuery replyChannel ->
      location |> replyChannel.Reply
      return! files location
    | SecretFileNotification ->
      printfn "Logic error: SecretFileNotification while having the secret files."
      return! files location
    | ExplodingBombNotification ->
      return! dead location
    | WebMessage ((ctx, requestingAgentColor), replyChannel) ->
      let webPart = 
        match ctx.request.``method`` with
        | HttpMethod.GET -> 
          let referrer = 
            match ctx.request.header "referer" with 
            | Choice1Of2 url -> Some url
            | Choice2Of2 _ -> None
          let doc = getAlive resourceAgentColor referrer location requestingAgentColor true 
          let s = doc |> Json.serialize |> Json.format 
          Successful.OK s
        | _ ->
          RequestErrors.METHOD_NOT_ALLOWED "no"
      webPart |> replyChannel.Reply
      return! files location }

  and dead location = async {
    printfn "Agent %s died but was immediately resurrected." resourceAgentColor
    return! alive location }

  alive location
)
