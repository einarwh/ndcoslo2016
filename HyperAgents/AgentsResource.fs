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

let mutable registeredAgents : Map<AgentColor, Agent<AgentResource.Message>> = Map.empty

let agentRef = Agent<Message>.Start (fun inbox ->
  let rec loop() = async {
    System.Console.WriteLine("AgentsResource got a message")
    let! msg = inbox.Receive()
    match msg with 
    | Lookup (agentColor, replyChannel) ->
      System.Console.WriteLine("Do lookup " + agentColor)
      registeredAgents |> Map.toSeq |> printfn "%A"
      registeredAgents.TryFind agentColor |> replyChannel.Reply
    | Register (agentColor, agent) ->
      System.Console.WriteLine("Do register " + agentColor)
      registeredAgents <- registeredAgents.Add(agentColor, agent)
    return! loop()
  }
  loop()
)
