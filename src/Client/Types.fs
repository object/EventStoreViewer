module EventStoreViewer.Types

open System

type EventSource =
    | Omnibus
    | Radioarkiv
    | Subtitles
    | Potion
    | FailedMessages

type EventCollection = {
    EventSource : EventSource
    HasProgramId : bool
}

module Collections =
    let all = [
        { EventSource = Omnibus; HasProgramId = true }
        { EventSource = Radioarkiv; HasProgramId = true }
        { EventSource = Subtitles; HasProgramId = true }
        { EventSource = Potion; HasProgramId = false }
        { EventSource = FailedMessages; HasProgramId = false }
    ]

type SearchMode =
    | ByInterval
    | ByIdOrProgram

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
    IdOrProgram : string
    FromTime : string
    ToTime : string
    TimeZone : TimeZone
    SelectedCollections : EventSource list
    SearchStatus : SearchStatus
    SearchResult : SearchResult option
    SelectedEventIndex : int option
    Content : string }

module Model =
    let private isValidTime s =
        if String.IsNullOrWhiteSpace s then 
            false
        else 
            let result, _ = DateTime.TryParse s
            result

    let hasValidIdOrProgram (model : Model) =
        not (String.IsNullOrWhiteSpace model.IdOrProgram)

    let hasValidFromTime (model : Model) = isValidTime model.FromTime

    let hasValidToTime (model : Model) = isValidTime model.ToTime

    let hasValidInterval (model : Model) = 
        isValidTime model.FromTime && isValidTime model.ToTime && DateTime.Parse model.ToTime > DateTime.Parse model.FromTime

    let isValid (model : Model) =
        match model.SearchMode with
        | ByIdOrProgram -> hasValidIdOrProgram model && model.SelectedCollections.Length >= 1
        | ByInterval -> hasValidInterval model && model.SelectedCollections.Length = 1

type Msg =
    | LoadAppSettings
    | AppSettingsLoaded of Result<AppSettings,string>
    | AppSettingsError of exn
    | SearchModeByIdOrProgram
    | SearchModeByInterval
    | SearchIntervalOneHour
    | SearchIntervalFourHours
    | SearchIntervalOneDay
    | SearchIntervalCustom
    | ProgramIdChanged of string
    | FromTimeChanged of string
    | ToTimeChanged of string
    | CollectionChanged of EventSource
    | TimeZoneChanged of bool
    | StartSearch
    | SearchCompleted of Result<ServiceEvent array,string>
    | SearchError of exn
    | EventSelected of int
    | EventUnselected
    | LoadContent of int
    | ContentLoaded of  Result<EventContent,string>
    | LoadError of exn

type EventRow = 
    { 
        CollectionName : string
        EventId : string
        Description : string
        Date : string
        Time : string
    }
