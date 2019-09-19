module EventStoreViewer.UpdateUtils

open System
open Elmish
open Fetch.Types
open Thoth.Json

open Model

let [<Literal>] DateTimeFormatString = "yyyy-MM-ddTHH:mm:ss"

let getRunningTime (model : Model) =
    match model.SearchStatus with
    | Running (_, startTime) -> DateTime.Now - startTime
    | _ -> TimeSpan(0L)

let private getCollectionTableName (col : EventSource) = 
    col.ToString().ToLower()

// Custom error message
let private errorString (response: Response) =
    string response.Status + " " + response.StatusText + " for URL " + response.Url

let private fetchWithDecoder<'T> url (decoder: Decoder<'T>) init =
    GlobalFetch.fetch(RequestInfo.Url url, Fetch.requestProps init)
    |> Promise.bind (fun response ->
        if not response.Ok then
            errorString response |> Error |> Promise.lift
        else
            response.text() |> Promise.map (Decode.fromString decoder))

let inline private fetchAs<'T> url init =
    let decoder = Decode.Auto.generateDecoderCached<'T>()
    fetchWithDecoder url decoder init

let private getSearchByIdUrl host collectionNames id =
    sprintf "%s/queryById/v1/%s/%s?noContent=true" host collectionNames id

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
    | ById -> 
        let collectionNames = String.Join (",", model.SelectedCollections |> List.map getCollectionTableName)
        getSearchByIdUrl model.AppSettings.apiUrl collectionNames (model.Id.Trim())
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
        | ById -> DateTime.MinValue, DateTime.MinValue 
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
