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
  | WebMessage of HttpContext * AsyncReplyChannel<WebPart>
  | LocationUpdate of Uri
  | SecretFileNotification
  | ExplodingBombNotification

let getAlive (agentColor : string) maybeReferrer =
  printfn "get alive representation of the %s agent" agentColor
  let links = 
    match maybeReferrer with
    | None -> []
    | Some ref -> [ { rel = ["back"]; href = ref } ]
  let doc = 
    { properties = { title = sprintf "The %s agent" agentColor; description = sprintf "You are the brillant and sly %s agent." agentColor }
      actions = []
      links = links }
  doc

let createAgent (color : string) (location : Uri) = 
  System.Console.WriteLine("Create agent: " + color)
  Agent<Message>.Start (fun inbox ->
  let rec alive (location : Uri) = async {
    System.Console.WriteLine("requested agent")
    let! msg = inbox.Receive()
    printfn "Agent %s got a message" color
    match msg with
    | LocationUpdate loc ->
      printfn "Location update for agent %s: %s" color <| loc.ToString()
      return! alive loc
    | SecretFileNotification ->
      return! files location
    | ExplodingBombNotification ->
      return! dead location
    | WebMessage (ctx, replyChannel) ->
      printfn "Agent %s got a web message." color
      let webPart = 
        match ctx.request.``method`` with
        | HttpMethod.GET ->
          let referrer = 
            match ctx.request.header "referer" with 
            | Choice1Of2 url -> Some url
            | Choice2Of2 _ -> None
          let s = getAlive color referrer |> Json.serialize |> Json.format 
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
    | SecretFileNotification ->
      System.Console.WriteLine("Logic error: SecretFileNotification while having the secret files.")
      return! files location
    | ExplodingBombNotification ->
      return! dead location
    | WebMessage (ctx, replyChannel) ->
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
      return! dead loc
    | SecretFileNotification ->
      System.Console.WriteLine("Logic error: SecretFileNotification while dead.")
      return! files location
    | ExplodingBombNotification ->
      System.Console.WriteLine("Logic error: ExplodingBombNotification while dead.")
      return! dead location
    | WebMessage (ctx, replyChannel) ->
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
