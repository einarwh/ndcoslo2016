module TrappableRoomWebPartResource

open System
open Suave
open Suave.Operators

open Chiron

open Utils
open Siren
open BombResource
open RoomResourceUtils
open TrappableRoomResource

type WebPartRoomRequestInfo = HttpContext * AgentColor * Agent<AgentResource.Message>

type WebPartRoomMessage = WebPartRoomRequestInfo * AsyncReplyChannel<WebPart>

let createAgent (roomInfo : RoomInfo) = 
  Agent<WebPartRoomMessage>.Start (fun inbox ->
  let trappableRoomAgent = TrappableRoomResource.createAgent roomInfo
  let rec loop() = async {
    let! (input, replyChannel) = inbox.Receive()
    let! res = trappableRoomAgent.PostAndAsyncReply(fun ch -> (input, ch))
    let webPart = 
      match res with 
      | Ok doc ->
        let s = doc |> Json.serialize |> Json.format
        Successful.OK s 
      | Created (doc, loc) ->
        let s = doc |> Json.serialize |> Json.format
        Successful.CREATED s >=> Writers.addHeader "location" (loc |> uri2str)
      | Found loc ->
        Redirection.FOUND (loc |> uri2str)
      | BadRequest why ->
        RequestErrors.BAD_REQUEST why
      | MethodNotAllowed why ->
        RequestErrors.METHOD_NOT_ALLOWED why 
      | InternalError why ->
        ServerErrors.INTERNAL_ERROR why

    webPart |> replyChannel.Reply 

    return! loop()        
    }

  loop()
)

