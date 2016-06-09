module ControlRoomResource

open Utils
open TrappableRoomWebPartResource

let roomInfo : RoomResourceUtils.RoomInfo = 
  { name = "control-room"
    properties = 
      { title = "The control room." 
        description = "The room you have entered is full of screens, buttons, flashing lights and beeping. Should you press a button? Which one?" }
    linkInfos = 
      [ ("office", ["entrance"; "move"])
        ("laboratory", ["entrance"; "move"])
        ("exit-room", ["entrance"; "move"]) ]
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
