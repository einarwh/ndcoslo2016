module ControlRoomResource

open System
open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful

open Chiron

open Utils
open Siren
open BombsResource
open RoomResourceUtils

type Agent<'T> = MailboxProcessor<'T>
type RequestInfo = HttpContext * AgentColor

type Message = WebMessage of RequestInfo * AsyncReplyChannel<WebPart> | DisarmNotification of string
type TrappedResult = SafeEntry of SirenDocument | TriggeredBomb of Uri

let getRoom (ctx : HttpContext) : SirenDocument =
  let roomName = "control-room"
  let props = 
    { title = "The Control Room."
      description = "You're in the control room." }
  let linkInfos =
    [ ("office", ["entrance"; "move"]) 
      ("laboratory", ["entrance"; "move"])
      ("exit-room", ["entrance"; "move"]) ]
  let doc = RoomResourceUtils.getRoomWithActions ctx.request props roomName linkInfos
  doc

let getTrapped (bombs : (int * string) list) (ctx : HttpContext) : TrappedResult = 
  match ctx.request.header "referer" with
  | Choice1Of2 ref ->
    match bombs |> List.tryFind (fun (id, b) -> ref.StartsWith(b)) with
    | None ->
      getRoom ctx |> SafeEntry
    | Some (id, bomb) ->
      let bombResourceUrl = sprintf "http://localhost:8083/bombs/%d?agent=%s" id "black"
      bombResourceUrl |> toUri |> TriggeredBomb
  | _ ->
    getRoom ctx |> SafeEntry
    
let agentRef = Agent<Message>.Start (fun inbox ->

  let rec loop (bombs : (int * string) list) = async {
    let! msg = inbox.Receive()
    printf "/control-room.\n"
    printf "Bombs: %A\n" bombs
    match msg with
    | DisarmNotification ref ->
      System.Console.WriteLine("disarmed!!!")
      return! bombs |> List.filter (fun (id, b) -> b = ref) |> loop
    | WebMessage ((ctx, clr), replyChannel) ->
      printfn "control-room-qp: %s" ctx.request.rawQuery
      match ctx.request.``method`` with
      | HttpMethod.GET -> 
        match getTrapped bombs ctx with
        | SafeEntry doc ->
          let s = doc |> Json.serialize |> Json.format
          Successful.OK s |> replyChannel.Reply
          return! loop bombs
        | TriggeredBomb loc ->
          Redirection.FOUND (loc.ToString()) |> replyChannel.Reply
          return! loop bombs
      | HttpMethod.POST ->
        match ctx.request.formData "bomb-referrer" with
        | Choice1Of2 ref ->
          (* Must create bomb resource and provide location header *)
          printfn "Should Register with BombsResource."
          let target = ctx.request.url |> uri2str
          let! bombId = BombsResource.agentRef.PostAndAsyncReply(fun ch -> BombsResource.Register(BombResource.createAgent ref target, ch))
          printfn "Got bomb id."
          let bombResourceUrl = sprintf "http://localhost:8083/bombs/%d" bombId
          let urlWithQuery = bombResourceUrl |> toUri |> withQueryString ("agent=" + "black") |> uri2str
          let doc = getRoom ctx
          let s = doc |> Json.serialize |> Json.format
          Successful.CREATED s >=> Writers.addHeader "location" urlWithQuery |> replyChannel.Reply
          return! loop ((bombId, ref) :: bombs)
        | Choice2Of2 why ->
          RequestErrors.BAD_REQUEST "no" |> replyChannel.Reply
          return! loop bombs
      | _ -> 
        RequestErrors.METHOD_NOT_ALLOWED "no" |> replyChannel.Reply
        return! loop bombs
      return! loop bombs
    }

  loop []
)
