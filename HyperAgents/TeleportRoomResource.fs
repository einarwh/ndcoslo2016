﻿module TeleportRoomResource

open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful

open Chiron

open Utils
open Siren

type RequestInfo = HttpContext
type ResponseInfo =
      | Success of SirenDocument
      | Failure

type Message = WebMessage of RequestInfo * AsyncReplyChannel<WebPart> | DisarmNotification
type TrappedResult = SafeEntry of SirenDocument | TriggeredBomb of string

let getRoom ctx = 
  let qp url = url + "?" + ctx.request.rawQuery 
  let links = 
    [ "teleport-room" |> qp |> selfLinkTo
      "laboratory" |> qp |> sirenLinkTo ["entrance"; "move"]
      "exit-room" |> qp |> sirenLinkTo ["teleport"; "move"] ] 
  let trappableLinks = 
    links |> List.filter (fun { rel = relations; href = lnk } -> relations |> List.exists (fun r -> r = "entrance"))
  let plantBombAction = 
    { name = "plant-bomb"
      title = "Plant bomb"
      ``method`` = "POST"
      href = ctx.request.url.ToString()
      fields = []
    }
  let pickResourceName (url : string) = 
    url.Split('?') |> Seq.head |> (fun s -> s.Split('/')) |> Seq.last

  let plantBombActions =
    trappableLinks 
    |> List.map (fun sl -> sl.href)
    |> List.mapi (fun i lnk -> 
      let srcResource = pickResourceName lnk 
      let dstResource = pickResourceName (ctx.request.url.ToString())
      { plantBombAction with name = sprintf "%s-%d" plantBombAction.name i
                             title = sprintf "Place bomb on entrance %s => %s" srcResource dstResource  
                             fields = [ { name = "bomb-referrer"; ``type`` = "text"; value = Some (String lnk)} ]})

  System.Console.WriteLine(trappableLinks |> List.map (fun sl -> sl.href))
  let doc = 
    { properties = 
        { title = "The Teleportation Room."
          description = "You're in the teleportation room. There's a teleport here." }
      actions = plantBombActions
      links = links }
  doc

let getTrapped (ctx : HttpContext) = 
  match ctx.request.header "referer" with
  | Choice1Of2 "http://localhost:8083/room1" -> 
    linkTo "bomb" |> TriggeredBomb
  | _ -> 
    getRoom ctx |> SafeEntry
    
let agentRef = Agent<Message>.Start (fun inbox ->

  let rec trapped() = async {
    let! msg = inbox.Receive()
    match msg with
    | DisarmNotification ->
      System.Console.WriteLine("disarmed!!!")
      return! cleared()
    | WebMessage (ctx, replyChannel) ->
      let webPart =
        match ctx.request.``method`` with
        | HttpMethod.GET -> 
          match getTrapped ctx with
          | SafeEntry doc ->
            let s = doc |> Json.serialize |> Json.format
            Successful.OK s
          | TriggeredBomb loc ->
            Redirection.FOUND loc
        | _ -> 
          RequestErrors.METHOD_NOT_ALLOWED "no"
      webPart |> replyChannel.Reply
    return! trapped()        
    }

  and cleared() = async {
    let! msg = inbox.Receive()
    match msg with
    | DisarmNotification ->
      return! cleared()
    | WebMessage (ctx, replyChannel) ->
      let webPart =
        match ctx.request.``method`` with
        | HttpMethod.GET -> 
          let s = getRoom ctx |> Json.serialize |> Json.format
          Successful.OK s
        | _ -> 
          RequestErrors.METHOD_NOT_ALLOWED "no"
      webPart |> replyChannel.Reply
      return! cleared()        
    }

  trapped()
)