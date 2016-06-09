module AgentsResource

open System

open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful

open Chiron
open Utils
open Siren

type Message = 
  | Lookup of AgentColor * AsyncReplyChannel<Agent<AgentResource.Message> option>
  | Register of AgentColor * Agent<AgentResource.Message>
  | ListAgents of Uri * AsyncReplyChannel<AgentColor list>

let agentRef = Agent<Message>.Start (fun inbox ->
  let rec loop (agents : Map<AgentColor, Agent<AgentResource.Message>>) = async {
    let! msg = inbox.Receive()
    match msg with 
    | Lookup (agentColor, replyChannel) ->
      agents.TryFind agentColor |> replyChannel.Reply
      return! loop agents
    | Register (agentColor, agent) ->
      printfn "Register agent %s" agentColor
      return! loop <| agents.Add(agentColor, agent)
    | ListAgents (url, replyChannel) ->
      let agentList = agents |> Map.toList
      let temp = agentList |> List.map (fun (color, agentRef) -> agentRef.PostAndAsyncReply(fun ch -> AgentResource.LocationQuery ch))
      let! urlsArray = temp |> Async.Parallel
      let urls = urlsArray |> Array.toList
      let here = 
        List.zip agentList urls |> 
        List.filter (fun (tup, u) -> (u.ToString()) = (url.ToString())) |>
        List.map (fun ((clr, a), u) -> clr)
      here |> replyChannel.Reply
      return! loop agents
      
  }
  loop Map.empty
)
