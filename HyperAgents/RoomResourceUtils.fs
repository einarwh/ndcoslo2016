module RoomResourceUtils

open System
open Chiron
open Suave
open Siren

type RoomInfo = 
  { name : string
    properties : SirenProperties
    linkInfos : (string * string list) list }

let getRoomWithActions1 (req : HttpRequest) (props : SirenProperties) (self : string) (linkInfos : (string * string list) list) = 
  let qp url = url + "?" + req.rawQuery 
  let links = 
    (self |> qp |> selfLinkTo) :: (linkInfos |> List.map (fun (name, rels) -> name |> qp |> sirenLinkTo rels))
  let trappableLinks = 
    links |> List.filter (fun { rel = relations; href = lnk } -> relations |> List.exists (fun r -> r = "entrance"))
  let plantBombAction = 
    { name = "plant-bomb"
      title = "Plant bomb"
      ``method`` = "POST"
      href = req.url.ToString()
      fields = []
    }
  let pickResourceName (url : string) = 
    url.Split('?') |> Seq.head |> (fun s -> s.Split('/')) |> Seq.last

  let plantBombActions =
    let noqp (uri : Uri) = new Uri(sprintf "%s://%s%s" uri.Scheme uri.Authority uri.AbsolutePath) 
    trappableLinks 
    |> List.map (fun sl -> new System.Uri(sl.href) |> noqp)
    |> List.mapi (fun i lnk -> 
      let srcResource = lnk.ToString() |> pickResourceName 
      let dstResource = (req.url |> noqp).ToString() |> pickResourceName
      { plantBombAction with name = sprintf "%s-%d" plantBombAction.name i
                             title = sprintf "Place bomb on entrance %s => %s" srcResource dstResource  
                             fields = [ { name = "bomb-referrer"; ``type`` = "text"; value = Some (String <| lnk.ToString())} ]})

  let doc = 
    { properties = props 
      actions = plantBombActions
      links = links }
  doc

let getRoomWithActions (req : HttpRequest) (roomInfo : RoomInfo) = 
  getRoomWithActions1 req roomInfo.properties roomInfo.name roomInfo.linkInfos
