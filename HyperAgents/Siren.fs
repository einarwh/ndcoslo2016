module Siren

open Chiron

type SirenLinkRel = string
type SirenHref = string

type SirenField = 
  { name: string
    ``type``: string
    value: Json option }

  static member ToJson (x: SirenField) = json {
    if x.value.IsSome then do! Json.write "value" x.value.Value
    do! Json.write "field" x.``type``
    do! Json.write "name" x.name
  }  

type SirenAction = 
  { name: string
    title: string
    ``method``: string
    href: SirenHref
    fields: SirenField list }

  static member ToJson (x: SirenAction) = json {
    do! Json.write "fields" x.fields
    do! Json.write "href" x.href
    do! Json.write "method" x.``method``
    do! Json.write "title" x.title
    do! Json.write "name" x.name
  }

type SirenLink = 
  { rel: SirenLinkRel list
    href: SirenHref }

  static member ToJson (x: SirenLink) = json {
    do! Json.write "href" x.href
    do! Json.write "rel" x.rel
  }

type SirenProperties = 
  { title: string
    description: string }

  static member ToJson (x: SirenProperties) = json {
    do! Json.write "description" x.description
    do! Json.write "title" x.title
  }

type SirenDocument = 
  { properties: SirenProperties
    actions: SirenAction list
    links: SirenLink list }

  static member ToJson (x: SirenDocument) = json {
    do! Json.write "properties" x.properties
    do! Json.write "actions" x.actions
    do! Json.write "links" x.links
  }
