module LaboratoryResource

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

let bombs = []

let getRoom ctx = 
  let qp = ctx.request.rawQuery
  System.Console.WriteLine(qp)
  let doc = 
    { properties = 
        { title = "The Laboratory."
          description = "You're in the laboratory." }
      actions = []
      links = 
        [ selfLinkTo "laboratory"
          sirenLinkTo ["entrance"; "move"] "teleport-room" 
          sirenLinkTo ["entrance"; "move"] "control-room" 
          sirenLinkTo ["entrance"; "move"] "office" ] }
  doc

let getTrapped (ctx : HttpContext) = 
  match ctx.request.header "referer" with
  | Choice1Of2 "http://localhost:8083/room1" -> 
    linkTo "bomb" |> TriggeredBomb
  | _ -> 
    getRoom ctx |> SafeEntry
  
let agentRef = Agent<Message>.Start (fun inbox ->

  let rec normal() = async {
    let! msg = inbox.Receive()
    match msg with
    | DisarmNotification ->
      System.Console.WriteLine("logic error, should never happen.")
      return! normal()
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
        | HttpMethod.POST ->
          RequestErrors.METHOD_NOT_ALLOWED "no"  
        | _ -> 
          RequestErrors.METHOD_NOT_ALLOWED "no"
      webPart |> replyChannel.Reply
    return! normal()        
    }

  and triggered() = async {
    let! msg = inbox.Receive()
    match msg with
    | DisarmNotification ->
      return! triggered()
    | WebMessage (ctx, replyChannel) ->
      let webPart =
        match ctx.request.``method`` with
        | HttpMethod.GET -> 
          let s = getRoom ctx |> Json.serialize |> Json.format
          Successful.OK s
        | _ -> 
          RequestErrors.METHOD_NOT_ALLOWED "no"
      webPart |> replyChannel.Reply
      return! triggered()        
    }

  normal()
)
