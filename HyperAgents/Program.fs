open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful

open Siren
open VoidResource

let bing: WebPart =
  fun (x : HttpContext) ->
    async {
      let! result = VoidResource.agentRef.PostAndAsyncReply(fun ch -> (x, ch))
      return! result x
    }

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

let altBombPart : WebPart =
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
      let! result = AltBombResource.agentRef.PostAndAsyncReply(fun ch -> (ctx, ch))
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
            >=> bombPart
        RequestErrors.METHOD_NOT_ALLOWED "I'm afraid I can't let you do that."
      ]
    path "/altbomb" >=>
      choose [
        GET >=> Writers.setMimeType "application/vnd.siren+json"
            >=> altBombPart
        POST >=> Writers.setMimeType "application/vnd.siren+json"
            >=> altBombPart
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
