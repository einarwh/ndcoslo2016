module OfficeResource

open Utils

let roomInfo : RoomResourceUtils.RoomInfo = 
  { name = "office"
    properties = 
      { title = "The office." 
        description = "You're in an office. There are screens on the walls. You see charts and burndowns. Your old arch-nemesis. Jira." }
    linkInfos = 
      [ ("laboratory", ["entrance"; "move"])
        ("control-room", ["entrance"; "move"]) ]
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
