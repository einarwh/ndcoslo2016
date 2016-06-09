module ExitRoomResource

open Chiron
open Suave
open Suave.Operators
open Suave.Utils
open Siren
open TrappableRoomResource
open Utils

type RequestInfo = HttpContext * AgentColor * Agent<AgentResource.Message>
type Message = RequestInfo * AsyncReplyChannel<WebPart>

let roomInfo : RoomResourceUtils.RoomInfo = 
  { name = "exit-room"
    properties = 
      { title = "The exit room." 
        description = "You're in the exit room. Do you have the secret files? Then you should get out of here!" }
    linkInfos = 
      [ ("control-room", ["entrance"; "move"]) ]
  }

let agentRef = Agent<Message>.Start (fun inbox ->

  let addPlaneLink (doc : SirenDocument) =
    let planeLink = { rel = [ "move" ]; href = linkTo "plane"}
    let links' = doc.links @ [planeLink]
    { doc with links = links' }

  let trappableRoomAgent = createAgent roomInfo
  let rec loop() = async {
    let! ((ctx, clr, agent), replyChannel) = inbox.Receive()
    let! res = trappableRoomAgent.PostAndAsyncReply(fun ch -> ((ctx, clr, agent), ch))
    (* Ask secret file where it is. *)
    let! fileAt = SecretFileResource.agentRef.PostAndAsyncReply(fun ch -> SecretFileResource.LocationQuery ch)
    let planeAvailable = 
      match fileAt with
      | SecretFileResource.SecretFileLocation.TakenByAgent agentColor 
        when agentColor = clr ->
        true
      | SecretFileResource.SecretFileLocation.TakenByAgent agentColor ->
        printfn "D'oh the %s agent has the secret file." agentColor
        false
      | SecretFileResource.SecretFileLocation.RoomLocation roomLoc ->
        printfn "No one has the secret file - no one can leave."
        false
    let webPart = 
      match res with 
      | Ok doc ->
        let doc' = if planeAvailable then addPlaneLink doc else doc
        let s = doc' |> Json.serialize |> Json.format
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
