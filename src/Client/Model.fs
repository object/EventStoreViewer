module EventStoreViewer.Model

open System
open Fable.Core

type EventSource =
    | Omnibus
    | Radioarkiv
    | Subtitles
    | Potion
    | FailedMessages

type EventCollection = {
    EventSource : EventSource
    HasCommonId : bool
}

module Collections =
    let all = [
        { EventSource = Omnibus; HasCommonId = true }
        { EventSource = Radioarkiv; HasCommonId = true }
        { EventSource = Subtitles; HasCommonId = true }
        { EventSource = Potion; HasCommonId = false }
        { EventSource = FailedMessages; HasCommonId = false }
    ]

type SearchMode =
    | ByInterval
    | ById

type SearchInterval =
    | OneHour
    | FourHours
    | OneDay
    | Custom

type SearchStatus =
    | Idle of string
    | Running of string * DateTime

[<RequireQualifiedAccess>]
type TimeZone =
    | Utc
    | Local

type ServiceEvent = 
    { 
        Collection : string option
        Id : string option
        ProgramId : string option
        CarrierId : string option
        ServiceName : string option
        Description : string option
        Created : DateTime
        IdPartitionKey : string
        DatePartitionKey : string
        RowKey : string
    }

type EventContent =
    { 
        Content : obj
    }

[<RequireQualifiedAccess>]
type ContentType =
    | None
    | Json
    | Xml

type SearchResult = {
    SearchMode : SearchMode
    Collections : EventSource list
    FromTime : DateTime
    ToTime : DateTime
    Events : ServiceEvent list }

type AppSettings = {
    apiUrl : string
    authorizationKey : string
}

type Model = { 
    AppSettings : AppSettings
    SearchMode : SearchMode
    SearchInterval : SearchInterval
    Id : string
    FromTime : string
    ToTime : string
    TimeZone : TimeZone
    SelectedCollections : EventSource list
    SearchStatus : SearchStatus
    SearchResult : SearchResult option
    SelectedEventIndex : int option
    Content : obj } with
    member this.ContentType =
        if isNull this.Content then
            ContentType.None
        else
            let text = (string)this.Content
            if text = "" then ContentType.None
            else if text.TrimStart().StartsWith "<" then ContentType.Xml
            else ContentType.Json
    member this.ContentAsString =
        match this.ContentType with
        | ContentType.None -> ""
        | ContentType.Json -> JS.JSON.stringify this.Content |> (fun x -> x.Trim('"'))
        | ContentType.Xml -> (string)this.Content

module Model =
    let private isValidTime s =
        if String.IsNullOrWhiteSpace s then 
            false
        else 
            let result, _ = DateTime.TryParse s
            result

    let hasValidId (model : Model) =
        not (String.IsNullOrWhiteSpace model.Id)

    let hasValidFromTime (model : Model) = isValidTime model.FromTime

    let hasValidToTime (model : Model) = isValidTime model.ToTime

    let hasValidInterval (model : Model) = 
        isValidTime model.FromTime && isValidTime model.ToTime && DateTime.Parse model.ToTime > DateTime.Parse model.FromTime

    let isValid (model : Model) =
        match model.SearchMode with
        | ById -> hasValidId model && model.SelectedCollections.Length >= 1
        | ByInterval -> hasValidInterval model && model.SelectedCollections.Length = 1
