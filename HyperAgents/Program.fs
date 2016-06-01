open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful

open Siren
open VoidResource

let agents = Map.empty

let voidPart : WebPart =
  fun (ctx : HttpContext) ->
    let acceptHeader = ctx.request.header "Accept"
    let hmm = 
      match acceptHeader with
        | Choice1Of2 a ->
          let comment = if a.Contains("html") then "oooh html" else "something else"
          comment
        | Choice2Of2 b -> b
    System.Console.WriteLine(hmm)
    async {
      let! result = VoidResource.agentRef.PostAndAsyncReply(fun ch -> (ctx, ch))
      return! result ctx
    }    

let bombPart : WebPart =
  fun (ctx : HttpContext) ->
    let acceptHeader = ctx.request.header "Accept"
    let hmm = 
      match acceptHeader with
        | Choice1Of2 a ->
          let comment = if a.Contains("html") then "oooh html" else "something else"
          comment
        | Choice2Of2 b -> b
    System.Console.WriteLine(hmm)
    async {
      let! result = BombResource.agentRef.PostAndAsyncReply(fun ch -> (ctx, ch))
      return! result ctx
    }    

let startPart : WebPart =
  fun (ctx : HttpContext) ->
    let acceptHeader = ctx.request.header "Accept"
    let hmm = 
      match acceptHeader with
        | Choice1Of2 a ->
          let comment = if a.Contains("html") then "oooh html" else "something else"
          comment
        | Choice2Of2 b -> b
    System.Console.WriteLine(hmm)
    async {
      let! result = StartResource.agentRef.PostAndAsyncReply(fun ch -> (ctx, ch))
      return! result ctx
    }    

let startAgentPart : WebPart =
  fun (ctx : HttpContext) ->
    async {
      let! result = StartResource.agentRef.PostAndAsyncReply(fun ch -> (ctx, ch))
      return! result ctx
    }    


let startroomPart : WebPart =
  fun (ctx : HttpContext) ->
    let acceptHeader = ctx.request.header "Accept"
    let hmm = 
      match acceptHeader with
        | Choice1Of2 a ->
          let comment = if a.Contains("html") then "oooh html" else "something else"
          comment
        | Choice2Of2 b -> b
    System.Console.WriteLine(hmm)
    async {
      let! result = StartRoomResource.agentRef.PostAndAsyncReply(fun ch -> (ctx, ch))
      return! result ctx
    }    

let controlRoomPart : WebPart =
  fun (ctx : HttpContext) ->
    let acceptHeader = ctx.request.header "Accept"
    let hmm = 
      match acceptHeader with
        | Choice1Of2 a ->
          let comment = if a.Contains("html") then "oooh html" else "something else"
          comment
        | Choice2Of2 b -> b
    System.Console.WriteLine(hmm)
    async {
      let! result = ControlRoomResource.agentRef.PostAndAsyncReply(fun ch -> ControlRoomResource.WebMessage (ctx, ch))
      return! result ctx
    }    

let officePart : WebPart =
  fun (ctx : HttpContext) ->
    let acceptHeader = ctx.request.header "Accept"
    let hmm = 
      match acceptHeader with
        | Choice1Of2 a ->
          let comment = if a.Contains("html") then "oooh html" else "something else"
          comment
        | Choice2Of2 b -> b
    System.Console.WriteLine(hmm)
    async {
      let! result = OfficeResource.agentRef.PostAndAsyncReply(fun ch -> OfficeResource.WebMessage (ctx, ch))
      return! result ctx
    }    

let teleportRoomPart : WebPart =
  fun (ctx : HttpContext) ->
    let acceptHeader = ctx.request.header "Accept"
    let hmm = 
      match acceptHeader with
        | Choice1Of2 a ->
          let comment = if a.Contains("html") then "oooh html" else "something else"
          comment
        | Choice2Of2 b -> b
    System.Console.WriteLine(hmm)
    async {
      let! result = TeleportRoomResource.agentRef.PostAndAsyncReply(fun ch -> TeleportRoomResource.WebMessage (ctx, ch))
      return! result ctx
    }    

let laboratoryPart : WebPart =
  fun (ctx : HttpContext) ->
    let acceptHeader = ctx.request.header "Accept"
    let hmm = 
      match acceptHeader with
        | Choice1Of2 a ->
          let comment = if a.Contains("html") then "oooh html" else "something else"
          comment
        | Choice2Of2 b -> b
    System.Console.WriteLine(hmm)
    async {
      let! result = LaboratoryResource.agentRef.PostAndAsyncReply(fun ch -> LaboratoryResource.WebMessage (ctx, ch))
      return! result ctx
    }    

let exitRoomPart : WebPart =
  fun (ctx : HttpContext) ->
    let acceptHeader = ctx.request.header "Accept"
    let hmm = 
      match acceptHeader with
        | Choice1Of2 a ->
          let comment = if a.Contains("html") then "oooh html" else "something else"
          comment
        | Choice2Of2 b -> b
    System.Console.WriteLine(hmm)
    async {
      let! result = ExitRoomResource.agentRef.PostAndAsyncReply(fun ch -> ExitRoomResource.WebMessage (ctx, ch))
      return! result ctx
    }    

let trapEntrancePart : WebPart =
  fun (ctx : HttpContext) ->
    let acceptHeader = ctx.request.header "Accept"
    let hmm = 
      match acceptHeader with
        | Choice1Of2 a ->
          let comment = if a.Contains("html") then "oooh html" else "something else"
          comment
        | Choice2Of2 b -> b
    System.Console.WriteLine(hmm)
    async {
      let! result = BoobyTrappedRoomResource.agentRef.PostAndAsyncReply(fun ch -> BoobyTrappedRoomResource.WebMessage (ctx, ch))
      return! result ctx
    }    

let agentsPart : WebPart = 
  fun (ctx : HttpContext) ->
    async {
      System.Console.WriteLine("agentsPart: " + ctx.request.url.ToString())
      match ctx.request.queryParam "agent" with
      | Choice1Of2 agentColor -> 
        System.Console.WriteLine("Should try lookup of " + agentColor)
        let! maybeAgent = AgentsResource.agentRef.PostAndAsyncReply(fun ch -> AgentsResource.Lookup (agentColor, ch))
        match maybeAgent with 
        | None ->
          return! RequestErrors.NOT_FOUND "no" <|ctx
        | Some agent ->
          let! result = agent.PostAndAsyncReply(fun ch -> AgentResource.WebMessage(ctx, ch))
          return! result ctx
      | Choice2Of2 x ->
        System.Console.WriteLine(x)
        return! RequestErrors.BAD_REQUEST "missing agent param" <| ctx 
    }

let app =
  choose [ 
    path "/void" >=> 
      choose [ 
        GET >=> Writers.setMimeType "application/json" 
            >=> voidPart
        POST >=> Writers.setMimeType "application/json" 
             >=> voidPart
        RequestErrors.METHOD_NOT_ALLOWED "I'm afraid I can't let you do that."
      ] 
    path "/bomb" >=>
      choose [
        GET >=> Writers.setMimeType "application/vnd.siren+json"
            >=> bombPart
        POST >=> Writers.setMimeType "application/vnd.siren+json"
            >=> bombPart >=> GET
        RequestErrors.METHOD_NOT_ALLOWED "I'm afraid I can't let you do that."
      ]
    path "/start" >=> 
      choose [ 
        GET >=> Writers.setMimeType "application/vnd.siren+json" 
            >=> startPart
        POST >=> Writers.setMimeType "application/vnd.siren+json" 
            >=> startPart
        RequestErrors.METHOD_NOT_ALLOWED "I'm afraid I can't let you do that."
      ] 
    path "/startroom" >=> 
      choose [ 
        GET >=> Writers.setMimeType "application/vnd.siren+json" 
            >=> startroomPart
        RequestErrors.METHOD_NOT_ALLOWED "I'm afraid I can't let you do that."
      ] 
    path "/control-room" >=> 
      choose [ 
        GET >=> Writers.setMimeType "application/vnd.siren+json" 
            >=> controlRoomPart
        RequestErrors.METHOD_NOT_ALLOWED "I'm afraid I can't let you do that."
      ] 
    path "/office" >=> 
      choose [ 
        GET >=> Writers.setMimeType "application/vnd.siren+json" 
            >=> officePart
        RequestErrors.METHOD_NOT_ALLOWED "I'm afraid I can't let you do that."
      ] 
    path "/teleport-room" >=> 
      choose [ 
        GET >=> Writers.setMimeType "application/vnd.siren+json" 
            >=> teleportRoomPart
        RequestErrors.METHOD_NOT_ALLOWED "I'm afraid I can't let you do that."
      ] 
    path "/laboratory" >=> 
      choose [ 
        GET >=> Writers.setMimeType "application/vnd.siren+json" 
            >=> laboratoryPart
        RequestErrors.METHOD_NOT_ALLOWED "I'm afraid I can't let you do that."
      ] 
    path "/exit-room" >=> 
      choose [ 
        GET >=> Writers.setMimeType "application/vnd.siren+json" 
            >=> exitRoomPart
        RequestErrors.METHOD_NOT_ALLOWED "I'm afraid I can't let you do that."
      ] 
    path "/agents" >=> 
      choose [ 
        GET >=> Writers.setMimeType "application/vnd.siren+json" 
            >=> agentsPart
        RequestErrors.METHOD_NOT_ALLOWED "I'm afraid I can't let you do that."
      ] 
    path "/room" >=> 
      choose [ 
        GET >=> Writers.setMimeType "application/vnd.siren+json" 
            >=> trapEntrancePart
        RequestErrors.METHOD_NOT_ALLOWED "I'm afraid I can't let you do that."
      ] 
    path "/goodbye" >=> 
      choose [ 
        GET >=> OK "Goodbye!! GET" 
        POST >=> OK "Goodbye!! POST" 
        RequestErrors.METHOD_NOT_ALLOWED "I'm afraid I can't let you do that."
      ]
  ]

let config = 
  { defaultConfig with 
      bindings   = [ HttpBinding.mkSimple HTTP "127.0.0.1" 1773 ] }

startWebServer defaultConfig app
