module PlaneResource

open Chiron
open Suave 
open Siren
open Utils
open RoomResourceUtils

type Message = HttpContext * AsyncReplyChannel<WebPart>

let doc : SirenDocument = 
  { properties = 
      { title = "The getaway plane!" 
        description = "LOL you won IDK. The other agents will blow up or something." }
    actions = []
    links = [ selfLinkTo "plane" 
              sirenLinkTo [ "restart" ] "start" ] }

let agentRef = 
  Agent<Message>.Start (fun inbox ->
    let rec loop() = async {
       let! (ctx, replyChannel) = inbox.Receive()
       doc |> Json.serialize |> Json.format |> Successful.OK |> replyChannel.Reply
       return! loop()
    }
    loop ())

