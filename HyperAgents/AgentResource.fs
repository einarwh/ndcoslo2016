module AgentResource

open Chiron

open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful

open Siren
open Utils

type Message =
  | WebMessage of HttpContext * AsyncReplyChannel<WebPart>
  | SecretFileNotification
  | ExplodingBombNotification

let getAlive agentColor maybeReferrer = 
  let links = 
    match maybeReferrer with
    | None -> []
    | Some ref -> [ { rel = ["back"]; href = ref } ]
  let doc = 
    { properties = { title = sprintf "The %s agent" agentColor; description = sprintf "You are the brillant and sly %s agent." agentColor }
      actions = [ ]
      links = links }
  doc

let createAgent (color : string) = 
  System.Console.WriteLine("Create agent: " + color)
  Agent<Message>.Start (fun inbox ->
  let rec alive (color : string) = async {
    System.Console.WriteLine("requested agent")
    let! msg = inbox.Receive()
    
    match msg with
    | SecretFileNotification ->
      return! files color
    | ExplodingBombNotification ->
      return! dead color
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
      return! alive color }
  
  and files color = async {
    let! msg = inbox.Receive()
    match msg with
    | SecretFileNotification ->
      System.Console.WriteLine("Logic error: SecretFileNotification while having the secret files.")
      return! files color
    | ExplodingBombNotification ->
      return! dead color
    | WebMessage (ctx, replyChannel) ->
      let webPart = 
        match ctx.request.``method`` with
        | HttpMethod.GET -> 
          Successful.OK "get [with files]"
        | _ ->
          RequestErrors.METHOD_NOT_ALLOWED "no"
      webPart |> replyChannel.Reply
      return! files color }

  and dead color = async {
    let! msg = inbox.Receive()
    match msg with
    | SecretFileNotification ->
      System.Console.WriteLine("Logic error: SecretFileNotification while dead.")
      return! files color
    | ExplodingBombNotification ->
      System.Console.WriteLine("Logic error: ExplodingBombNotification while dead.")
      return! dead color
    | WebMessage (ctx, replyChannel) ->
      let webPart = 
        match ctx.request.``method`` with
        | HttpMethod.GET -> 
          Successful.OK "get [dead]"
        | _ ->
          RequestErrors.METHOD_NOT_ALLOWED "no"
      webPart |> replyChannel.Reply
      return! dead color }

  alive color
)
