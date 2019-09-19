module EventStoreViewer.ViewUtils

open System
open Fable.React

open Model

let [<Literal>] DateFormatString = "yyyy-MM-dd"
let [<Literal>] TimeFormatString = "HH:mm:ss"

let inline prettifyJson json : ReactElement =
  ofImport "default" "react-json-view"
    {|
      src = json
      name = false
      enableClipboard = false
      displayObjectSize = false
      displayDataTypes = false
    |}
    []

let inline prettifyXml xml : ReactElement =
  ofImport "default" "react-xml-viewer"
    {| xml = xml |} []

type EventRow = 
    { 
        CollectionName : string
        EventId : string
        Description : string
        Date : string
        Time : string
    }

let rec private getCommonRange xs ys result =
    match (xs, ys) with
    | [], _ | _, [] -> result
    | x :: xs, y :: ys ->
        if x = y 
        then getCommonRange xs ys (result @ [x]) 
        else result
        
let private getCommonSubstring (s1 : string) (s2 : string) =
    String.Concat(Array.ofList(getCommonRange (s1 |> Seq.toList) (s2 |> Seq.toList) []))

let getCollectionDisplayName collection =
    match collection with
    | Omnibus -> "Omnibus"
    | Radioarkiv -> "Radioarkiv"
    | Subtitles -> "Subtitles"
    | Potion -> "Potion"
    | FailedMessages -> "Failed Messages"

let createResultRow (result : SearchResult) (event : ServiceEvent) (model : Model) =
  let collectionName = 
    match event.Collection with 
    | Some name -> name 
    | _ -> result.Collections |> List.head |> fun x -> x.ToString().ToLower()
  let eventId = 
    match (event.Id, event.ProgramId, event.ServiceName) with
    | Some id, _, Some serviceName -> sprintf "%s (%s)" id serviceName
    | None, Some programId, Some serviceName -> sprintf "%s (%s)" programId serviceName
    | Some id, _, _ -> id
    | None, Some programId, _ ->
        match event.CarrierId with
        | Some carrierId when carrierId <> programId -> 
            let commonSubstring = getCommonSubstring programId carrierId
            sprintf "%s (%s)" programId (carrierId.Substring(commonSubstring.Length))
        | _ -> programId
    | None, None, _ -> "EMPTY"
  let description = 
    match event.Description with 
    | Some text -> text
    | _ -> ""
  let date = 
    if result.SearchMode = ById || result.FromTime.Date <> result.ToTime.Date then 
       event.Created.ToString(DateFormatString) 
    else 
      ""
  {
    CollectionName = collectionName
    EventId = eventId
    Description = description
    Date = date
    Time = event.Created.ToString(TimeFormatString)
  }
