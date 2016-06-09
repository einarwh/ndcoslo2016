module LaboratoryResource

open Utils
open TrappableRoomWebPartResource

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
  
let agentRef = Agent<WebPartRoomMessage>.Start (fun inbox ->
  let trappableRoomAgent = createAgent roomInfo
  let rec loop() = async {
    let! (input, replyChannel) = inbox.Receive()
    let! response = trappableRoomAgent.PostAndAsyncReply(fun ch -> (input, ch))
    response |> replyChannel.Reply
    return! loop()        
    }

  loop()
)
