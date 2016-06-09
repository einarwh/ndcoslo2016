module TrappableRoomResource

open System
open Suave
open Suave.Operators

open Chiron

open Utils
open Siren
open BombResource
open RoomResourceUtils

type RoomRequestInfo = HttpContext * AgentColor * Agent<AgentResource.Message>

type ResponseMessage = 
  | Ok of SirenDocument 
  | Created of SirenDocument * Uri
  | Found of Uri
  | BadRequest of string
  | MethodNotAllowed of string
  | InternalError of string

type RoomMessage = RoomRequestInfo * AsyncReplyChannel<ResponseMessage>
type TrappedResult = SafeEntry of SirenDocument | TriggeredBomb of (int * Uri)

let addOtherAgentsIfPresent others roomInfo = 
  let enemyLinkInfo enemy = 
    let enemyPath = sprintf "agents/%s" enemy
    (enemyPath, [ "enemy" ])
  match others with
  | [] -> roomInfo
  | [a] -> 
    let props = roomInfo.properties
    let addition = sprintf " That pesky %s agent is here." a 
    let linkInfos' = roomInfo.linkInfos @ [ enemyLinkInfo a ]
    { roomInfo with properties = { props with description = props.description + addition }
                    linkInfos = linkInfos' }
  | lst ->
    let props = roomInfo.properties
    let addition = lst |> List.fold (fun acc a -> ", " + a) "" |> sprintf " The following pesky agents are here: %s."
    let enemyLinkInfos = lst |> List.map (fun a -> enemyLinkInfo a)
    let linkInfos' = roomInfo.linkInfos @ enemyLinkInfos
    { roomInfo with properties = { props with description = props.description + addition }
                    linkInfos = linkInfos' }

let addSecretFileIfPresent secretFileIsHere roomInfo = 
  if secretFileIsHere then
    let props = roomInfo.properties
    let addition = " But wait - you have found the secret file! Grab it!"
    let properties' = { props with description = props.description + addition }
    let secretFileLinkInfo = ("secret-file", [ "look" ])
    let linkInfos' = roomInfo.linkInfos @ [ secretFileLinkInfo ]
    { roomInfo with properties = properties'
                    linkInfos = linkInfos' }
  else
    roomInfo

let getRoom 
      (ctx : HttpContext) 
      (clr : AgentColor) 
      (others : AgentColor list) 
      (roomInfo : RoomInfo) 
      (secretFileIsHere) : SirenDocument =
  let roomInfo' = roomInfo |> addOtherAgentsIfPresent others |> addSecretFileIfPresent secretFileIsHere
  RoomResourceUtils.getRoomWithActions ctx.request clr roomInfo'

let bombMatch (agentRef : string) (bombRef : string) =
  let pathOf ref = ref |> toUri |> withoutQueryString |> justPath
  pathOf agentRef = pathOf bombRef
 
let getTrapped 
     (bombs : BombInfo list) 
     (ctx : HttpContext) 
     (clr : AgentColor) 
     (others : AgentColor list) 
     (roomInfo : RoomInfo)
     (secretFileIsHere : bool): TrappedResult = 
  match ctx.request.header "referer" with
  | Choice1Of2 ref ->
    match bombs |> List.tryFind (fun { id = id; referrer = referrer; agent = agent } -> bombMatch ref referrer) with
    | None ->
      getRoom ctx clr others roomInfo secretFileIsHere |> SafeEntry
    | Some { id = id; referrer = referrer; agent = agent } ->
      let bombResourceUrl = sprintf "http://localhost:8083/bombs/%d?agent=%s" id clr
      let bomb = bombResourceUrl |> toUri 
      TriggeredBomb (id, bombResourceUrl |> toUri)
  | _ ->
    getRoom ctx clr others roomInfo secretFileIsHere |> SafeEntry

let createAgent (roomInfo : RoomInfo) = 
  Agent<RoomMessage>.Start (fun inbox ->
    let rec loop (bombs : BombInfo list) = async {
      printfn "room %s is awaiting messages" roomInfo.name
      let! ((ctx, clr, agentAgent), replyChannel) = inbox.Receive()
      agentAgent.Post(AgentResource.LocationUpdate(ctx.request.url |> withoutQueryString))

      printfn "url: %A" ctx.request.url
      let justUrl = ctx.request.url |> withoutQueryString
      let! agentsPresent = AgentsResource.agentRef.PostAndAsyncReply(fun ch -> AgentsResource.ListAgents(justUrl, ch))
      printfn "These are here: %A (%d)" agentsPresent <| List.length agentsPresent
      let otherAgents = agentsPresent |> List.filter (fun c -> c <> clr)
      printfn "Other agents here: %A (%d)" otherAgents <| List.length otherAgents

      (* Is the secret file here? *)
      let! fileLocation = SecretFileResource.agentRef.PostAndAsyncReply(fun ch -> SecretFileResource.LocationQuery ch)
      let secretFileIsHere = 
        match fileLocation with
        | SecretFileResource.RoomLocation roomLoc ->
          (justUrl |> justPath) = (roomLoc |> justPath)
        | SecretFileResource.TakenByAgent takenByAgent -> false

      printfn "current bomb count: %d" <| List.length bombs 
      let temp = bombs |> List.map (fun {id = id; referrer = referrer; agent = bombAgent} ->  
        bombAgent.PostAndAsyncReply(fun ch -> BombResource.AliveQuery ch))
      let! livenessArray = temp |> Async.Parallel
      let aliveness = livenessArray |> Array.toList 
      let activeBombs = List.zip bombs aliveness |> List.filter (fun (b, alive) -> alive) |> List.map (fun (b, alive) -> b)
      printfn "active bomb count: %d" <| List.length activeBombs 
      match ctx.request.``method`` with
      | HttpMethod.GET -> 
        match getTrapped activeBombs ctx clr otherAgents roomInfo secretFileIsHere with
        | SafeEntry doc ->
          Ok doc |> replyChannel.Reply
          return! loop activeBombs
        | TriggeredBomb (bombId, loc) ->
          let! bomb = BombsResource.agentRef.PostAndAsyncReply(fun ch -> BombsResource.Lookup(bombId, ch))
          match bomb with
          | None ->
            let err = "Logic messed up: triggered a bomb that doesn't exist."
            InternalError err |> replyChannel.Reply
          | Some b ->
            b.Post(BombResource.TriggerNotification)
            Found loc |> replyChannel.Reply
          return! loop activeBombs
      | HttpMethod.POST ->
        match ctx.request.formData "bomb-referrer" with
        | Choice1Of2 ref ->
          (* Must create bomb resource and provide location header *)
          let target = ctx.request.url |> uri2str
          let! bombId = BombsResource.agentRef.PostAndAsyncReply(fun ch -> BombsResource.Register(BombResource.createAgent ref target, ch))
          let! bombAgent = BombsResource.agentRef.PostAndAsyncReply(fun ch -> BombsResource.Lookup(bombId, ch))
          printfn "Got bomb id."
          let bombResourceUrl = sprintf "http://localhost:8083/bombs/%d" bombId
          let urlWithQuery = bombResourceUrl |> toUri |> withQueryString ("agent=" + clr)
          let doc = getRoom ctx clr otherAgents roomInfo secretFileIsHere
          Created (doc, urlWithQuery) |> replyChannel.Reply
          let bomb = { id = bombId; referrer = ref; agent = bombAgent.Value }
          return! loop (bomb :: bombs)
        | Choice2Of2 why ->
          BadRequest why |> replyChannel.Reply
          return! loop bombs
      | _ -> 
        MethodNotAllowed "no" |> replyChannel.Reply
        return! loop bombs
      return! loop bombs
    }

    loop [])