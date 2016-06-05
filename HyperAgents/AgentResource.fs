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

let selfDescription resourceAgentColor path =
  sprintf "You are the brillant and sly %s agent. You are currently here: %s." resourceAgentColor path

let otherDescription resourceAgentColor =
  sprintf "It is the cowardly and deceitful %s agent, up to no good as usual." resourceAgentColor

let getAlive (resourceAgentColor : string) maybeReferrer (location : Uri) (requestingAgentColor : string) =
  printfn "get alive representation of the %s agent" resourceAgentColor
  printfn "requested by %s" requestingAgentColor
  let path = location.AbsolutePath.Substring(1)
  let links = 
    match maybeReferrer with
    | None -> []
    | Some ref -> [ { rel = ["back"]; href = ref } ]
  let desc = 
    if requestingAgentColor = resourceAgentColor then 
      selfDescription resourceAgentColor path
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
  System.Console.WriteLine("Create agent: " + resourceAgentColor)
  Agent<Message>.Start (fun inbox ->
  let rec alive (location : Uri) = async {
    System.Console.WriteLine("agent is alive!")
    let! msg = inbox.Receive()
    printfn "Agent %s got a message" resourceAgentColor
    match msg with
    | LocationUpdate loc ->
      printfn "Location update for agent %s: %s" resourceAgentColor <| loc.ToString()
      return! alive loc
    | LocationQuery replyChannel ->
      printfn "O agent %s where art thou?" resourceAgentColor
      location |> replyChannel.Reply
      return! alive location
    | SecretFileNotification ->
      return! files location
    | ExplodingBombNotification ->
      return! dead location
    | WebMessage ((ctx, requestingAgentColor), replyChannel) ->
      printfn "Agent %s got a web message." resourceAgentColor
      let webPart = 
        match ctx.request.``method`` with
        | HttpMethod.GET ->
          let referrer = 
            match ctx.request.header "referer" with 
            | Choice1Of2 url -> Some url
            | Choice2Of2 _ -> None
          let s = getAlive resourceAgentColor referrer location requestingAgentColor |> Json.serialize |> Json.format 
          Successful.OK s
        | _ ->
          RequestErrors.METHOD_NOT_ALLOWED "no"
      webPart |> replyChannel.Reply
      return! alive location }
  
  and files location = async {
    let! msg = inbox.Receive()
    match msg with
    | LocationUpdate loc ->
      return! files loc
    | LocationQuery replyChannel ->
      printfn "O agent %s where art thou?" resourceAgentColor
      location |> replyChannel.Reply
      return! files location
    | SecretFileNotification ->
      System.Console.WriteLine("Logic error: SecretFileNotification while having the secret files.")
      return! files location
    | ExplodingBombNotification ->
      return! dead location
    | WebMessage ((ctx, clr), replyChannel) ->
      let webPart = 
        match ctx.request.``method`` with
        | HttpMethod.GET -> 
          Successful.OK "get [with files]"
        | _ ->
          RequestErrors.METHOD_NOT_ALLOWED "no"
      webPart |> replyChannel.Reply
      return! files location }

  and dead location = async {
    let! msg = inbox.Receive()
    match msg with
    | LocationUpdate loc ->
      System.Console.WriteLine("Logic error: LocationUpdate while dead.")
      return! dead location
    | LocationQuery replyChannel ->
      printfn "O agent %s where art thou?" resourceAgentColor
      location |> replyChannel.Reply
      return! dead location
    | SecretFileNotification ->
      System.Console.WriteLine("Logic error: SecretFileNotification while dead.")
      return! files location
    | ExplodingBombNotification ->
      System.Console.WriteLine("Logic error: ExplodingBombNotification while dead.")
      return! dead location
    | WebMessage ((ctx, clr), replyChannel) ->
      let webPart = 
        match ctx.request.``method`` with
        | HttpMethod.GET -> 
          Successful.OK "get [dead]"
        | _ ->
          RequestErrors.METHOD_NOT_ALLOWED "no"
      webPart |> replyChannel.Reply
      return! dead location }

  alive location
)
