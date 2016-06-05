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

type RoomMessage = RoomRequestInfo * AsyncReplyChannel<WebPart>
type TrappedResult = SafeEntry of SirenDocument | TriggeredBomb of (int * Uri)

let getRoom (ctx : HttpContext) (clr : AgentColor) (others : AgentColor list) (roomInfo : RoomInfo) : SirenDocument =
  let enemyLinkInfo enemy = 
    let enemyPath = sprintf "agents/%s" enemy
    (enemyPath, [ "enemy" ])
  let roomInfo' =
    match others with
    | [] -> roomInfo
    | [a] -> 
      printfn "found another agent here..."
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
  RoomResourceUtils.getRoomWithActions ctx.request clr roomInfo'

let getTrapped (bombs : BombInfo list) (ctx : HttpContext) (clr : AgentColor) (others : AgentColor list) (roomInfo : RoomInfo): TrappedResult = 
  match ctx.request.header "referer" with
  | Choice1Of2 ref ->
    match bombs |> List.tryFind (fun { id = id; referrer = referrer; agent = agent } -> ref.StartsWith(referrer)) with
    | None ->
      getRoom ctx clr others roomInfo |> SafeEntry
    | Some { id = id; referrer = referrer; agent = agent } ->
      let bombResourceUrl = sprintf "http://localhost:8083/bombs/%d?agent=%s" id clr
      let bomb = bombResourceUrl |> toUri 
      TriggeredBomb (id, bombResourceUrl |> toUri)
  | _ ->
    getRoom ctx clr others roomInfo |> SafeEntry

let createAgent (roomInfo : RoomInfo) = 
  printfn "Create trappable room agent for %s." roomInfo.name
  Agent<RoomMessage>.Start (fun inbox ->
    let rec loop (bombs : BombInfo list) = async {
        let! ((ctx, clr, agentAgent), replyChannel) = inbox.Receive()
        agentAgent.Post(AgentResource.LocationUpdate(ctx.request.url |> withoutQueryString))

        printfn "url: %A" ctx.request.url
        let justUrl = ctx.request.url |> withoutQueryString
        let! agentsPresent = AgentsResource.agentRef.PostAndAsyncReply(fun ch -> AgentsResource.ListAgents(justUrl, ch))
        printfn "These are here: %A (%d)" agentsPresent <| List.length agentsPresent
        let otherAgents = agentsPresent |> List.filter (fun c -> c <> clr)
        printfn "Other agents here: %A (%d)" otherAgents <| List.length otherAgents

        printfn "current bomb count: %d" <| List.length bombs 
        let temp = bombs |> List.map (fun {id = id; referrer = referrer; agent = bombAgent} ->  
          bombAgent.PostAndAsyncReply(fun ch -> BombResource.AliveQuery ch))
        let! livenessArray = temp |> Async.Parallel
        let aliveness = livenessArray |> Array.toList 
        let activeBombs = List.zip bombs aliveness |> List.filter (fun (b, alive) -> alive) |> List.map (fun (b, alive) -> b)
        printfn "active bomb count: %d" <| List.length activeBombs 
        match ctx.request.``method`` with
        | HttpMethod.GET -> 
          match getTrapped activeBombs ctx clr otherAgents roomInfo with
          | SafeEntry doc ->
            let s = doc |> Json.serialize |> Json.format
            Successful.OK s |> replyChannel.Reply
            return! loop activeBombs
          | TriggeredBomb (bombId, loc) ->
            let! bomb = BombsResource.agentRef.PostAndAsyncReply(fun ch -> BombsResource.Lookup(bombId, ch))
            match bomb with
            | None ->
              ServerErrors.INTERNAL_ERROR "Logic messed up: triggered a bomb that doesn't exist." |> replyChannel.Reply
            | Some b ->
              b.Post(BombResource.TriggerNotification)
              Redirection.FOUND (loc.ToString()) |> replyChannel.Reply
            return! loop activeBombs
        | HttpMethod.POST ->
          match ctx.request.formData "bomb-referrer" with
          | Choice1Of2 ref ->
            (* Must create bomb resource and provide location header *)
            printfn "Should Register with BombsResource."
            let target = ctx.request.url |> uri2str
            let! bombId = BombsResource.agentRef.PostAndAsyncReply(fun ch -> BombsResource.Register(BombResource.createAgent ref target, ch))
            let! bombAgent = BombsResource.agentRef.PostAndAsyncReply(fun ch -> BombsResource.Lookup(bombId, ch))
            printfn "Got bomb id."
            let bombResourceUrl = sprintf "http://localhost:8083/bombs/%d" bombId
            let urlWithQuery = bombResourceUrl |> toUri |> withQueryString ("agent=" + clr) |> uri2str
            let doc = getRoom ctx clr otherAgents roomInfo
            let s = doc |> Json.serialize |> Json.format
            Successful.CREATED s >=> Writers.addHeader "location" urlWithQuery |> replyChannel.Reply
            let bomb = { id = bombId; referrer = ref; agent = bombAgent.Value }
            return! loop (bomb :: bombs)
          | Choice2Of2 why ->
            RequestErrors.BAD_REQUEST "no" |> replyChannel.Reply
            return! loop bombs
        | _ -> 
          RequestErrors.METHOD_NOT_ALLOWED "no" |> replyChannel.Reply
          return! loop bombs
        return! loop bombs
      }

    loop [])