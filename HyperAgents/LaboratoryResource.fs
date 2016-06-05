module LaboratoryResource

open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful

open Chiron

open Utils
open Siren


let roomInfo : RoomResourceUtils.RoomInfo = 
  { name = "laboratory"
    properties = 
      { title = "The laboratory." 
        description = "You're in a secret research laboratory. Gadgets are whizzing." }
    linkInfos = 
      [ ("teleport-room", ["entrance"; "move"])
        ("control-room", ["entrance"; "move"])
        ("office", ["entrance"; "move"]) ]
  }
  
let agentRef = Agent<TrappableRoomResource.RoomMessage>.Start (fun inbox ->
  let trappableRoomAgent = TrappableRoomResource.createAgent roomInfo
  let rec loop() = async {
    let! (input, replyChannel) = inbox.Receive()
    let! response = trappableRoomAgent.PostAndAsyncReply(fun ch -> (input, ch))
    response |> replyChannel.Reply
    return! loop()        
    }

  loop()
)
