module AgentsResource

open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful

open Chiron
open Utils
open Siren

type Agent<'T> = MailboxProcessor<'T>
type RequestInfo = HttpContext
type ResponseInfo =
      | Success of SirenDocument
      | Failure

type AgentColor = string
type Url = string

type Message = 
  | Lookup of AgentColor * AsyncReplyChannel<Agent<AgentResource.Message> option>
  | Register of AgentColor * Agent<AgentResource.Message>

let agentRef = Agent<Message>.Start (fun inbox ->
  let rec loop (agents : Map<AgentColor, Agent<AgentResource.Message>>) = async {
    System.Console.WriteLine("AgentsResource got a message")
    let! msg = inbox.Receive()
    match msg with 
    | Lookup (agentColor, replyChannel) ->
      System.Console.WriteLine("Do lookup " + agentColor)
      agents |> Map.toSeq |> printfn "%A"
      agents.TryFind agentColor |> replyChannel.Reply
      return! loop agents
    | Register (agentColor, agent) ->
      System.Console.WriteLine("Do register " + agentColor)
      return! loop <| agents.Add(agentColor, agent)
  }
  loop Map.empty
)
