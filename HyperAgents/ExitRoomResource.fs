module ExitRoomResource

open Utils

let roomInfo : RoomResourceUtils.RoomInfo = 
  { name = "exit-room"
    properties = 
      { title = "The exit room." 
        description = "You're in the exit room. Do you have the secret files? Then you should get out of here!" }
    linkInfos = 
      [ ("control-room", ["entrance"; "move"])
        ("plane", ["move"]) ]
  }
  
let agentRef = Agent<TrappableRoomResource.RoomMessage>.Start (fun inbox ->
  let trappableRoomAgent = TrappableRoomResource.createAgent roomInfo
  let rec loop() = async {
    let! (input, replyChannel) = inbox.Receive()
    let! response = trappableRoomAgent.PostAndAsyncReply(fun ch -> (input, ch))
    (* Ask agent-agent about the secret file? Or ask secret file about agent? *)
    response |> replyChannel.Reply
    return! loop()        
    }

  loop()
)
