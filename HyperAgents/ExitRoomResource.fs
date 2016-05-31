module ExitRoomResource

open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful

open Chiron

open Utils
open Siren

type Agent<'T> = MailboxProcessor<'T>
type RequestInfo = HttpContext
type ResponseInfo =
      | Success of SirenDocument
      | Failure

type Message = WebMessage of RequestInfo * AsyncReplyChannel<WebPart> | DisarmNotification
type TrappedResult = SafeEntry of SirenDocument | TriggeredBomb of string

let getRoom ctx = 
  let doc = 
    { properties = 
        { title = "The Exit Room."
          description = "You're in the exit room. Do you have the secret files? Then you should get out of here!" }
      actions = []
      links = [ selfLinkTo "exit-room"; sirenLinkTo ["move"] "plane" ] }
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
