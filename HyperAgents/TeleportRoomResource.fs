module TeleportRoomResource

open Utils
open TrappableRoomWebPartResource

let roomInfo : RoomResourceUtils.RoomInfo = 
  { name = "teleport"
    properties = 
      { title = "The teleportation room." 
        description = "You're in the teleportation room. Lo and behold, there a teleportation device here. Who would have guessed?" }
    linkInfos = 
      [ ("laboratory", ["entrance"; "move"])
        ("exit-room", ["teleport"; "move"]) ]
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
