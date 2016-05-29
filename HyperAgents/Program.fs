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
    path "/hello" >=> 
      choose [ 
        GET >=> Writers.setMimeType "application/json" 
            >=> bing
        POST >=> (" { \"key\": \"Hello!! POST\" } " |> OK)
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
