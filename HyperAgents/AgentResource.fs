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

let getAlive agentColor maybeReferrer = 
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
  let rec alive (color : string) (location : Uri) = async {
    System.Console.WriteLine("requested agent")
    let! msg = inbox.Receive()
    match msg with
    | LocationUpdate loc ->
      return! alive color loc
    | SecretFileNotification ->
      return! files color location
    | ExplodingBombNotification ->
      return! dead color location
    | WebMessage (ctx, replyChannel) ->
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
      return! alive color location }
  
  and files color location = async {
    let! msg = inbox.Receive()
    match msg with
    | LocationUpdate loc ->
      return! files color loc
    | SecretFileNotification ->
      System.Console.WriteLine("Logic error: SecretFileNotification while having the secret files.")
      return! files color location
    | ExplodingBombNotification ->
      return! dead color location
    | WebMessage (ctx, replyChannel) ->
      let webPart = 
        match ctx.request.``method`` with
        | HttpMethod.GET -> 
          Successful.OK "get [with files]"
        | _ ->
          RequestErrors.METHOD_NOT_ALLOWED "no"
      webPart |> replyChannel.Reply
      return! files color location }

  and dead color location = async {
    let! msg = inbox.Receive()
    match msg with
    | LocationUpdate loc ->
      System.Console.WriteLine("Logic error: LocationUpdate while dead.")
      return! dead color loc
    | SecretFileNotification ->
      System.Console.WriteLine("Logic error: SecretFileNotification while dead.")
      return! files color location
    | ExplodingBombNotification ->
      System.Console.WriteLine("Logic error: ExplodingBombNotification while dead.")
      return! dead color location
    | WebMessage (ctx, replyChannel) ->
      let webPart = 
        match ctx.request.``method`` with
        | HttpMethod.GET -> 
          Successful.OK "get [dead]"
        | _ ->
          RequestErrors.METHOD_NOT_ALLOWED "no"
      webPart |> replyChannel.Reply
      return! dead color location }

  alive color location
)
