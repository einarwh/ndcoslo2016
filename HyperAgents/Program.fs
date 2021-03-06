﻿open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful

open Siren
open Utils

let startPart : WebPart =
  fun (ctx : HttpContext) ->
    async {
      let! result = StartResource.agentRef.PostAndAsyncReply(fun ch -> (ctx, ch))
      return! result ctx
    }
    
let roomWithAgent (roomAgent : Agent<TrappableRoomWebPartResource.WebPartRoomMessage>) : WebPart =
  fun (ctx : HttpContext) ->
    let agentColor = ctx.request.queryParam "agent"
    async {
      match agentColor with 
      | Choice1Of2 clr ->
        let! maybeAgent = AgentsResource.agentRef.PostAndAsyncReply(fun ch -> AgentsResource.Lookup(clr, ch))
        match maybeAgent with
        | None ->
          return! RequestErrors.BAD_REQUEST (sprintf "no such agent %s" clr) ctx
        | Some agentAgent ->
          let! result = roomAgent.PostAndAsyncReply(fun ch -> ((ctx, clr, agentAgent), ch))
          return! result ctx
      | Choice2Of2 x ->
        return! RequestErrors.BAD_REQUEST x ctx 
    }    

let controlRoomPart : WebPart =
  roomWithAgent ControlRoomResource.agentRef

let officePart : WebPart =
  roomWithAgent OfficeResource.agentRef

let teleportRoomPart : WebPart =
  roomWithAgent TeleportRoomResource.agentRef

let laboratoryPart : WebPart =
  roomWithAgent LaboratoryResource.agentRef

let exitRoomPart : WebPart =
  roomWithAgent ExitRoomResource.agentRef

let secretFilePart : WebPart =
  fun (ctx : HttpContext) ->
    let agentColor = ctx.request.queryParam "agent"
    async {
      match agentColor with 
      | Choice1Of2 clr ->
        let! maybeAgent = AgentsResource.agentRef.PostAndAsyncReply(fun ch -> AgentsResource.Lookup(clr, ch))
        match maybeAgent with
        | None ->
          return! RequestErrors.BAD_REQUEST (sprintf "no such agent %s" clr) ctx
        | Some agentAgent ->
          let! result = SecretFileResource.agentRef.PostAndAsyncReply(fun ch -> SecretFileResource.WebMessage((ctx, clr), ch))
          return! result ctx
      | Choice2Of2 x ->
        return! RequestErrors.BAD_REQUEST x ctx 
    }    

let agentsPart : WebPart = 
  fun (ctx : HttpContext) ->
    async {
      match ctx.request.queryParam "agent" with
      | Choice1Of2 agentColor -> 
        printfn "Try lookup of %s" agentColor
        let! maybeAgent = AgentsResource.agentRef.PostAndAsyncReply(fun ch -> AgentsResource.Lookup (agentColor, ch))
        match maybeAgent with 
        | None ->
          return! RequestErrors.NOT_FOUND "no" <|ctx
        | Some agent ->
          let! result = agent.PostAndAsyncReply(fun ch -> AgentResource.WebMessage((ctx, agentColor), ch))
          return! result ctx
      | Choice2Of2 x ->
        return! RequestErrors.BAD_REQUEST x <| ctx 
    }

let agentPart agentResourceColor : WebPart = 
  fun (ctx : HttpContext) ->
    let requestingAgentColor = ctx.request.queryParam "agent"
    async {
      match requestingAgentColor with 
      | Choice1Of2 clr ->
        let! maybeRequestingAgent = AgentsResource.agentRef.PostAndAsyncReply(fun ch -> AgentsResource.Lookup(clr, ch))
        match maybeRequestingAgent with
        | None ->
          return! RequestErrors.BAD_REQUEST (sprintf "no such agent %s" clr) ctx
        | Some requestingAgentAgent ->
          let! maybeAgentResource = AgentsResource.agentRef.PostAndAsyncReply(fun ch -> AgentsResource.Lookup(agentResourceColor, ch))
          match maybeAgentResource with 
          | None ->
            return! RequestErrors.NOT_FOUND (sprintf "no such agent %s" agentResourceColor) <|ctx
          | Some agentResource ->
            let! result = agentResource.PostAndAsyncReply(fun ch -> AgentResource.WebMessage((ctx, clr), ch))
            return! result ctx
      | Choice2Of2 x ->
        return! RequestErrors.BAD_REQUEST x ctx 
    }

let bombPart (bombId : int) : WebPart =
  fun (ctx : HttpContext) ->
    async {
      printfn "Lookup bomb #%d" bombId
      let! maybeBomb = BombsResource.agentRef.PostAndAsyncReply(fun ch -> BombsResource.Lookup (bombId, ch))
      match maybeBomb with
      | None ->
        return! RequestErrors.NOT_FOUND "no such bomb" <| ctx
      | Some bomb -> 
        let! result = bomb.PostAndAsyncReply(fun ch -> BombResource.WebMessage(ctx, ch))
        return! result ctx
    }

let planePart : WebPart =
  fun (ctx : HttpContext) ->
    async {
      let! result = PlaneResource.agentRef.PostAndAsyncReply(fun ch -> (ctx, ch))
      return! result ctx
    }

let setMimeTypeSiren = 
  Writers.setMimeType "application/vnd.siren+json"

let nope = 
  RequestErrors.METHOD_NOT_ALLOWED "I'm afraid I can't let you do that."

let app =
  choose [ 
    pathScan "/bombs/%d" (fun bombId ->
      choose [
        GET >=> setMimeTypeSiren 
            >=> bombPart bombId
        POST >=> setMimeTypeSiren 
            >=> bombPart bombId
        nope
      ]
    )
    path "/start" >=> 
      choose [ 
        GET >=> setMimeTypeSiren 
            >=> startPart
        POST >=> setMimeTypeSiren 
            >=> startPart
        nope
      ] 
    path "/control-room" >=> 
      choose [ 
        GET >=> setMimeTypeSiren 
            >=> controlRoomPart
        POST >=> setMimeTypeSiren 
             >=> controlRoomPart
        nope
      ] 
    path "/office" >=> 
      choose [ 
        GET >=> setMimeTypeSiren 
            >=> officePart
        POST >=> setMimeTypeSiren 
            >=> officePart
        nope
      ] 
    path "/teleport-room" >=> 
      choose [ 
        GET >=> setMimeTypeSiren 
            >=> teleportRoomPart
        POST >=> setMimeTypeSiren 
             >=> teleportRoomPart
        nope
      ] 
    path "/laboratory" >=> 
      choose [ 
        GET >=> setMimeTypeSiren 
            >=> laboratoryPart
        POST >=> setMimeTypeSiren 
             >=> laboratoryPart
        nope
      ] 
    path "/exit-room" >=> 
      choose [ 
        GET >=> setMimeTypeSiren 
            >=> exitRoomPart
        nope
      ] 
    pathScan "/agents/%s" (fun agentResourceColor ->
      choose [
        GET >=> setMimeTypeSiren 
            >=> agentPart agentResourceColor
        nope
      ]) 
    path "/agents" >=> 
      choose [ 
        GET >=> setMimeTypeSiren 
            >=> agentsPart
        nope
      ] 
    path "/secret-file" >=> 
      choose [ 
        GET >=> setMimeTypeSiren 
            >=> secretFilePart
        POST >=> secretFilePart
        nope
      ] 
    path "/plane" >=>
      choose [
        GET >=> setMimeTypeSiren >=> planePart
        nope
      ]
    RequestErrors.NOT_FOUND "no such resource"
  ]

startWebServer defaultConfig app
