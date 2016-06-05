module Utils

open System

type Agent<'T> = MailboxProcessor<'T>
type AgentColor = string

let linkTo relativeUrl = "http://localhost:8083/" + relativeUrl

let link2 (path : string) (query : string) = 
  let uri = new UriBuilder("http://localhost:8083")
  uri.Path <- path
  uri.Query <- query.Substring(1)
  uri.ToString()

let toUri (s : string) : Uri = new Uri(s)

let uri2str (uri: Uri) : string = uri.ToString()

let withQueryString (qs : string) (uri : Uri) : Uri =
  let ub = new UriBuilder(uri)
  ub.Query <- qs
  ub.Uri

let withQueryAgent (agent : string) (url : Uri) : Uri = 
  let qs = sprintf "agent=%s" agent
  url |> withQueryString qs

let withoutQueryString (url : Uri) : Uri = 
  let s = String.Format("{0}{1}{2}{3}", url.Scheme, Uri.SchemeDelimiter, url.Authority, url.AbsolutePath);
  s |> toUri

let justPath (url : Uri) : string = 
  url.AbsolutePath

let r = new System.Random()

let getRandomRoom() : string =
  let locations = [ "control-room"; "office"; "laboratory"; "teleport-room"; "exit-room" ]
  let roomIndex = r.Next(List.length locations)
  locations.Item roomIndex

let getRandomStartLocation() : string =
  getRandomRoom() |> linkTo

