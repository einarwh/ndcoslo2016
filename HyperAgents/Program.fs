open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful

open Chiron

type SirenLinkRel = string
type SirenHref = string

type SirenField = 
  { name: string
    ``type``: string
    value: Json option }

  static member ToJson (x: SirenField) = json {
    if x.value.IsSome then do! Json.write "value" x.value.Value
    do! Json.write "field" x.``type``
    do! Json.write "name" x.name
  }  

type SirenAction = 
  { name: string
    title: string
    ``method``: string
    href: SirenHref
    fields: SirenField list }

  static member ToJson (x: SirenAction) = json {
    do! Json.write "fields" x.fields
    do! Json.write "href" x.href
    do! Json.write "method" x.``method``
    do! Json.write "title" x.title
    do! Json.write "name" x.name
  }

type SirenLink = 
  { rel: SirenLinkRel list
    href: SirenHref }
 
  static member ToJson (x: SirenLink) = json {
    do! Json.write "href" x.href
    do! Json.write "rel" x.rel
  }

type SirenProperties = 
  { title: string
    description: string }

  static member ToJson (x: SirenProperties) = json {
    do! Json.write "description" x.description
    do! Json.write "title" x.title
  }

type SirenDocument = 
  { properties: SirenProperties
    actions: SirenAction list
    links: SirenLink list }

  static member ToJson (x: SirenDocument) = json {
    do! Json.write "properties" x.properties
    do! Json.write "actions" x.actions
    do! Json.write "links" x.links
  }

[<AutoOpen>]
module VoidResource =  
    type Agent<'T> = MailboxProcessor<'T>
    type RequestInfo = HttpContext
    type ResponseInfo =
        | Success of SirenDocument
        | Failure
    type State = Foo | Bar | Quux

    type Message = RequestInfo * AsyncReplyChannel<WebPart>

    let updateState state msg = 
        match state with
        | Foo -> Bar
        | Bar -> Quux
        | Quux -> Foo

    let get ctx =  
       let nameField = { name = "name"; ``type`` = "text"; value = None }
       let classField = { name = "class"; ``type`` = "text"; value = None }
       let raceField = { name = "race"; ``type`` = "text"; value = None }

       let createAction = 
           { name = "start-adventure"
             ``method`` = "POST"
             title = "Start adventure"
             href = "http://void"
             fields = [nameField; classField; raceField] }

       let selfLink = { rel = [ "self" ]; href = "http://void" }

       let doc = 
           { properties = { title = "Void"; description = "The Magical Void" }
             actions = [ createAction ]
             links = [ selfLink ] }
       doc

    let agentRef = Agent<Message>.Start (fun inbox ->
            let rec loop oldState = async {
                let! msg = inbox.Receive()
                let (ctx, replyChannel) = msg

                let m = ctx.request.``method``
                let f = 
                  match m with 
                  | HttpMethod.GET -> "get"
                  | HttpMethod.POST -> "post"
                  | _ -> "other"

                System.Console.WriteLine(f)

                let state = updateState oldState ctx

                let doc = 
                  { properties = 
                      { title = "Void"
                        description = "The Magical Void" }
                    actions = []
                    links = [] }

                let webPart =
                  match m with 
                  | HttpMethod.GET -> 
                    let s = get ctx |> Json.serialize |> Json.format
                    Successful.OK s
                  | HttpMethod.POST -> 
                    let s = doc |> Json.serialize |> Json.format
                    Successful.CREATED s >=> Writers.addHeader "location" "http://localhost/whee"
                  | _ -> OK "huh"

                webPart |> replyChannel.Reply
                return! loop state
            }
            loop Foo
        )

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
