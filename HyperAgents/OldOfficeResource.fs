module OldOfficeResource

open System
open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful

open Chiron

open Utils
open Siren
open BombResource
open RoomResourceUtils

type Agent<'T> = MailboxProcessor<'T>
type RequestInfo = HttpContext * AgentColor

type Message = WebMessage of RequestInfo * AsyncReplyChannel<WebPart> | DisarmNotification of string
type TrappedResult = SafeEntry of SirenDocument | TriggeredBomb of (int * Uri)

let getRoom (ctx : HttpContext) (clr : AgentColor) : SirenDocument =
  let props = 
    { title = "The Office."
      description = "You're in an office." }
  let linkInfos =
    [ ("laboratory", ["entrance"; "move"]) 
      ("control-room", ["entrance"; "move"])
      ("agents/" + clr, ["me"]) ]
  let doc = RoomResourceUtils.getRoomWithActions1 ctx.request props "office" linkInfos
  doc

let getTrapped (bombs : BombInfo list) (ctx : HttpContext) (clr : AgentColor): TrappedResult = 
  match ctx.request.header "referer" with
  | Choice1Of2 ref ->
    match bombs |> List.tryFind (fun { id = id; referrer = referrer; agent = agent } -> ref.StartsWith(referrer)) with
    | None ->
      getRoom ctx clr |> SafeEntry
    | Some { id = id; referrer = referrer; agent = agent } ->
      let bombResourceUrl = sprintf "http://localhost:8083/bombs/%d?agent=%s" id clr
      let bomb = bombResourceUrl |> toUri 
      TriggeredBomb (id, bombResourceUrl |> toUri)
  | _ ->
    getRoom ctx clr |> SafeEntry
    
let agentRef = Agent<Message>.Start (fun inbox ->

  let rec loop (bombs : BombInfo list) = async {
    let! msg = inbox.Receive()
    printf "Office.\n"
    printf "Bombs: %A\n" bombs
    match msg with
    | DisarmNotification ref ->
      (* Probably not needed. Bombs disappear by themselves, sort of. *)
      printfn "disarm notification!!!"
      return! bombs |> loop
    | WebMessage ((ctx, clr), replyChannel) ->
      let! maybeAgent = AgentsResource.agentRef.PostAndAsyncReply(fun ch -> AgentsResource.Lookup(clr, ch))
      match maybeAgent with
      | None ->
        printfn "no agent?"
      | Some agent ->
        printfn "got an agent, yes."      
        agent.Post(AgentResource.LocationUpdate(ctx.request.url |> withoutQueryString))
      printfn "office-qp: %s" ctx.request.rawQuery
      printfn "current bomb count: %d" <| List.length bombs 
      let something = bombs |> List.map (fun {id = id; referrer = referrer; agent = agent} ->  
        agent.PostAndAsyncReply(fun ch -> BombResource.AliveQuery ch))
      let! livenessArray = something |> Async.Parallel
      let aliveness = livenessArray |> Array.toList 
      let activeBombs = List.zip bombs aliveness |> List.filter (fun (b, alive) -> alive) |> List.map (fun (b, alive) -> b)
      printfn "active bomb count: %d" <| List.length activeBombs 
                                           
      match ctx.request.``method`` with
      | HttpMethod.GET -> 
        match getTrapped activeBombs ctx clr with
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
          let doc = getRoom ctx clr
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

  loop []
)
