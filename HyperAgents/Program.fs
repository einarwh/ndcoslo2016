open Suave
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
    
let roomWithAgent (agent : Agent<TrappableRoomResource.RoomMessage>) : WebPart =
  fun (ctx : HttpContext) ->
    let agentColor = ctx.request.queryParam "agent"
    async {
      match agentColor with 
      | Choice1Of2 clr ->
        let! result = agent.PostAndAsyncReply(fun ch -> ((ctx, clr), ch))
        return! result ctx
      | Choice2Of2 x ->
        return! RequestErrors.BAD_REQUEST x ctx 
    }    

let controlRoomPart : WebPart =
  roomWithAgent ControlRoomResource.agentRef

let officePart : WebPart =
  fun (ctx : HttpContext) ->
    let agentColor = ctx.request.queryParam "agent"
    async {
      match agentColor with 
      | Choice1Of2 clr ->
        let! result = OfficeResource.agentRef.PostAndAsyncReply(fun ch -> OfficeResource.WebMessage ((ctx, clr), ch))
        return! result ctx
      | Choice2Of2 x ->
        return! RequestErrors.BAD_REQUEST x ctx 
    }    

let teleportRoomPart : WebPart =
  roomWithAgent TeleportRoomResource.agentRef

let laboratoryPart : WebPart =
  roomWithAgent LaboratoryResource.agentRef

let exitRoomPart : WebPart =
  fun (ctx : HttpContext) ->
    async {
      let! result = ExitRoomResource.agentRef.PostAndAsyncReply(fun ch -> ExitRoomResource.WebMessage (ctx, ch))
      return! result ctx
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
          let! result = agent.PostAndAsyncReply(fun ch -> AgentResource.WebMessage(ctx, ch))
          return! result ctx
      | Choice2Of2 x ->
        return! RequestErrors.BAD_REQUEST x <| ctx 
    }

let agentPart agentColor : WebPart = 
  fun (ctx : HttpContext) ->
    async {
      let! maybeAgent = AgentsResource.agentRef.PostAndAsyncReply(fun ch -> AgentsResource.Lookup (agentColor, ch))
      match maybeAgent with 
      | None ->
        return! RequestErrors.NOT_FOUND "no" <|ctx
      | Some agent ->
        let! result = agent.PostAndAsyncReply(fun ch -> AgentResource.WebMessage(ctx, ch))
        return! result ctx
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

let setMimeTypeSiren = 
  Writers.setMimeType "application/vnd.siren+json"

let app =
  choose [ 
    pathScan "/bombs/%d" (fun bombId ->
      choose [
        GET >=> setMimeTypeSiren 
            >=> bombPart bombId
        POST >=> setMimeTypeSiren 
            >=> bombPart bombId
        RequestErrors.METHOD_NOT_ALLOWED "I'm afraid I can't let you do that."
      ]
    )
    path "/start" >=> 
      choose [ 
        GET >=> setMimeTypeSiren 
            >=> startPart
        POST >=> setMimeTypeSiren 
            >=> startPart
        RequestErrors.METHOD_NOT_ALLOWED "I'm afraid I can't let you do that."
      ] 
    path "/control-room" >=> 
      choose [ 
        GET >=> setMimeTypeSiren 
            >=> controlRoomPart
        POST >=> setMimeTypeSiren 
             >=> controlRoomPart
        RequestErrors.METHOD_NOT_ALLOWED "I'm afraid I can't let you do that."
      ] 
    path "/office" >=> 
      choose [ 
        GET >=> setMimeTypeSiren 
            >=> officePart
        POST >=> setMimeTypeSiren 
            >=> officePart
        RequestErrors.METHOD_NOT_ALLOWED "I'm afraid I can't let you do that."
      ] 
    path "/teleport-room" >=> 
      choose [ 
        GET >=> setMimeTypeSiren 
            >=> teleportRoomPart
        POST >=> setMimeTypeSiren 
             >=> teleportRoomPart
        RequestErrors.METHOD_NOT_ALLOWED "I'm afraid I can't let you do that."
      ] 
    path "/laboratory" >=> 
      choose [ 
        GET >=> setMimeTypeSiren 
            >=> laboratoryPart
        POST >=> setMimeTypeSiren 
             >=> laboratoryPart
        RequestErrors.METHOD_NOT_ALLOWED "I'm afraid I can't let you do that."
      ] 
    path "/exit-room" >=> 
      choose [ 
        GET >=> setMimeTypeSiren 
            >=> exitRoomPart
        RequestErrors.METHOD_NOT_ALLOWED "I'm afraid I can't let you do that."
      ] 
    pathScan "/agents/%s" (fun agentColor ->
      choose [
        GET >=> setMimeTypeSiren 
            >=> agentPart agentColor
        RequestErrors.METHOD_NOT_ALLOWED "I'm afraid I can't let you do that."
      ]) 
    path "/agents" >=> 
      choose [ 
        GET >=> setMimeTypeSiren 
            >=> agentsPart
        RequestErrors.METHOD_NOT_ALLOWED "I'm afraid I can't let you do that."
      ] 
  ]

startWebServer defaultConfig app
