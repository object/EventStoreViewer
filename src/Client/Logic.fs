module EventStoreViewer.Logic

open System
open Elmish
open Fable.Core
open Fetch.Types
open Thoth.Json

open Types

let [<Literal>] DateTimeFormatString = "yyyy-MM-ddTHH:mm:ss"
let [<Literal>] DateFormatString = "yyyy-MM-dd"
let [<Literal>] TimeFormatString = "HH:mm:ss"

let rec getCommonRange xs ys result =
    match (xs, ys) with
    | [], _ | _, [] -> result
    | x :: xs, y :: ys ->
        if x = y 
        then getCommonRange xs ys (result @ [x]) 
        else result
        
let getCommonSubstring (s1 : string) (s2 : string) =
    String.Concat(Array.ofList(getCommonRange (s1 |> Seq.toList) (s2 |> Seq.toList) []))

let getRunningTime (model : Model) =
    match model.SearchStatus with
    | Running (_, startTime) -> DateTime.Now - startTime
    | _ -> TimeSpan(0L)

let getCollectionTableName (col : EventSource) = 
    col.ToString().ToLower()

let getCollectionDisplayName collection =
    match collection with
    | Omnibus -> "Omnibus"
    | Radioarkiv -> "Radioarkiv"
    | Subtitles -> "Subtitles"
    | Potion -> "Potion"
    | FailedMessages -> "Failed Messages"

// Custom error message
let errorString (response: Response) =
    string response.Status + " " + response.StatusText + " for URL " + response.Url

let fetchWithDecoder<'T> url (decoder: Decoder<'T>) init =
    GlobalFetch.fetch(RequestInfo.Url url, Fetch.requestProps init)
    |> Promise.bind (fun response ->
        if not response.Ok then
            errorString response |> Error |> Promise.lift
        else
            response.text() |> Promise.map (Decode.fromString decoder))

let inline fetchAs<'T> url init =
    let decoder = Decode.Auto.generateDecoderCached<'T>()
    fetchWithDecoder url decoder init

let private getSearchByIdOrProgramUrl host collectionNames idOrProgram =
    sprintf "%s/queryById/v1/%s/%s?noContent=true" host collectionNames idOrProgram

let private getSearchByIntervalUrl host collectionName fromTime toTime =
    sprintf "%s/queryByTime/v1/%s/%s/%s?noContent=true" host collectionName fromTime toTime

let private getLoadContentUrl host collectionName partitionKey rowKey =
    sprintf "%s/getByKey/v1/%s/%s/%s" host collectionName partitionKey rowKey

let getAppSettings model onCompleted onError =
    let fetchSettings () = fetchAs<AppSettings> "appSettings.json" []
    { model with SearchStatus = SearchStatus.Running ("Loading appSettings.json", DateTime.Now ) }, 
    Cmd.OfPromise.either fetchSettings () onCompleted onError

let getSearchUrl (model : Model) =
    let offset = if DateTime.Now.IsDaylightSavingTime() then 2 else 1
    let getLocalTime (time : DateTime) = time.AddHours(-float offset)
    match model.SearchMode with
    | ByIdOrProgram -> 
        let collectionNames = String.Join (",", model.SelectedCollections |> List.map getCollectionTableName)
        getSearchByIdOrProgramUrl model.AppSettings.apiUrl collectionNames (model.IdOrProgram.Trim())
    | ByInterval -> 
        let collectionName = model.SelectedCollections |> List.head |> getCollectionTableName
        let now = DateTime.Now
        let fromTime,toTime =
            match model.SearchInterval with
            | OneHour -> now.AddHours -1., now
            | FourHours -> now.AddHours -4., now
            | OneDay -> now.AddDays -1., now
            | Custom -> DateTime.Parse model.FromTime, DateTime.Parse model.ToTime
            |> fun (fromTime, toTime) ->
                match model.TimeZone with
                | TimeZone.Utc -> fromTime, toTime
                | TimeZone.Local -> getLocalTime fromTime, getLocalTime toTime
            |> fun (fromTime, toTime) -> 
                fromTime.ToString(DateTimeFormatString), toTime.ToString(DateTimeFormatString)
        getSearchByIntervalUrl model.AppSettings.apiUrl collectionName fromTime toTime

let getContentUrl (model : Model) (serviceEvent : ServiceEvent) =
    let collectionName = 
        match serviceEvent.Collection with
        | Some collection -> collection
        | None -> model.SelectedCollections |> List.head |> getCollectionTableName
    getLoadContentUrl model.AppSettings.apiUrl collectionName serviceEvent.IdPartitionKey serviceEvent.RowKey

let startSearch (model : Model) fetchUrl onCompleted onError =
    let authorizedFetchUrl = sprintf "%s&code=%s" fetchUrl model.AppSettings.authorizationKey
    let fetchEvents () = fetchAs<ServiceEvent []> authorizedFetchUrl []
    let statusMessage = sprintf "Retrieving results from\n%s\n%s" model.AppSettings.apiUrl 
                                (fetchUrl.Substring(model.AppSettings.apiUrl.Length))
    let fromTime, toTime =
        match model.SearchMode with 
        | ByIdOrProgram -> DateTime.MinValue, DateTime.MinValue 
        | ByInterval -> DateTime.Parse model.FromTime, DateTime.Parse model.ToTime
    { model with 
        SearchStatus = SearchStatus.Running (statusMessage, DateTime.Now )
        SearchResult = Some { 
            SearchResult.SearchMode = model.SearchMode
            Collections = model.SelectedCollections
            FromTime = fromTime
            ToTime = toTime
            Events = [] }
        SelectedEventIndex = None
        Content = "" }, Cmd.OfPromise.either fetchEvents () onCompleted onError

let loadContent (model : Model) fetchUrl onCompleted onError =
    let authorizedFetchUrl = sprintf "%s?code=%s" fetchUrl model.AppSettings.authorizationKey
    let fetchContent () = fetchAs<EventContent> authorizedFetchUrl []
    let statusMessage = sprintf "Retrieving content from\n%s\n%s" model.AppSettings.authorizationKey 
                                (fetchUrl.Substring(model.AppSettings.apiUrl.Length))
    { model with 
        SearchStatus = SearchStatus.Running (statusMessage, DateTime.Now )
        Content = "" }, Cmd.OfPromise.either fetchContent () onCompleted onError

let extractContent (serviceEvent : EventContent) =
    let text = (string)serviceEvent.Content
    if text.TrimStart().StartsWith "<?xml" then
        text
    else
        JS.JSON.stringify serviceEvent.Content |> (fun x -> x.Trim('"'))

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
    if result.SearchMode = ByIdOrProgram || result.FromTime.Date <> result.ToTime.Date then 
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
