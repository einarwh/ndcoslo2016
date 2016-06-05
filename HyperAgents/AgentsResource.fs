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
    System.Console.WriteLine("AgentsResource got a message")
    let! msg = inbox.Receive()
    match msg with 
    | Lookup (agentColor, replyChannel) ->
      printfn "Lookup agent %s" agentColor
      agents |> Map.toSeq |> printfn "%A"
      agents.TryFind agentColor |> replyChannel.Reply
      return! loop agents
    | Register (agentColor, agent) ->
      printfn "Register agent %s" agentColor
      return! loop <| agents.Add(agentColor, agent)
    | ListAgents (url, replyChannel) ->
      printfn "List agents at %s" <| url.ToString()
      let agentList = agents |> Map.toList
      let temp = agentList |> List.map (fun (color, agentRef) -> agentRef.PostAndAsyncReply(fun ch -> AgentResource.LocationQuery ch))
      let! urlsArray = temp |> Async.Parallel
      let urls = urlsArray |> Array.toList
      let here = 
        List.zip agentList urls |> 
        List.filter (fun (tup, u) -> (u.ToString()) = (url.ToString())) |>
        List.map (fun ((clr, a), u) -> clr)
      printfn "Found %d agents." <| List.length here
      here |> replyChannel.Reply
      return! loop agents
      
  }
  loop Map.empty
)
