module EventStoreViewer.State

open System
open Elmish

open Types
open Logic

let init _ =
    let now = DateTime.Now
    let model = {
        AppSettings = { apiUrl = ""; authorizationKey = "" }
        SearchMode = ByInterval
        SearchInterval = OneHour
        IdOrProgram = ""
        FromTime = now.AddHours(-1.).ToString(DateTimeFormatString)
        ToTime = now.ToString(DateTimeFormatString)
        TimeZone = TimeZone.Local
        SelectedCollections = []
        SearchStatus = Idle ""
        SearchResult = None
        SelectedEventIndex = None
        Content = ""
    }
    (model, Cmd.ofMsg LoadAppSettings)

let update msg model =

    let collectionHasProgramId collection =
        Collections.all |> List.exists (fun x -> x.EventSource = collection && x.HasProgramId)

    let collectionsHaveProgramId collections =
        collections |> List.forall collectionHasProgramId

    let updateSelectedCollections collection =
        if model.SelectedCollections |> List.contains collection then
            model.SelectedCollections |> List.filter (fun x -> x <> collection)
        else 
            match model.SearchMode with
            | ByIdOrProgram when collectionHasProgramId collection && collectionsHaveProgramId model.SelectedCollections -> collection :: model.SelectedCollections
            | ByIdOrProgram -> [collection]
            | ByInterval -> [collection]

    let getErrorMessage (exn : exn) =
        let elapsed = getRunningTime model
        sprintf "Error:\n%s\nLast request: %d ms" exn.Message (int elapsed.TotalMilliseconds) 

    match msg with
    | LoadAppSettings -> getAppSettings model AppSettingsLoaded AppSettingsError
    | AppSettingsLoaded result -> 
        match result with
        | Result.Ok settings ->
            { model with AppSettings = settings; SearchStatus = Idle "Ready to execute requests" }, Cmd.none
        | Result.Error error ->
            { model with SearchStatus = Idle error }, Cmd.none
    | AppSettingsError exn -> 
        { model with SearchStatus = Idle <| getErrorMessage exn }, Cmd.none
    | SearchModeByIdOrProgram -> 
        { model with 
            SearchMode = ByIdOrProgram
            SelectedCollections = if model.SelectedCollections.Length = 1 then model.SelectedCollections else [] 
        }, Cmd.none
    | SearchModeByInterval -> 
        { model with 
            SearchMode = ByInterval 
            SelectedCollections = if model.SelectedCollections.Length = 1 then model.SelectedCollections else [] 
        }, Cmd.none
    | ProgramIdChanged idOrProgram -> 
        { model with IdOrProgram = idOrProgram }, Cmd.none
    | SearchIntervalOneHour ->
        { model with SearchInterval = OneHour }, Cmd.none
    | SearchIntervalFourHours ->
        { model with SearchInterval = FourHours }, Cmd.none
    | SearchIntervalOneDay ->
        { model with SearchInterval = OneDay }, Cmd.none
    | SearchIntervalCustom ->
        { model with SearchInterval = Custom }, Cmd.none
    | FromTimeChanged time -> 
        { model with FromTime = time }, Cmd.none
    | ToTimeChanged time -> 
        { model with ToTime = time }, Cmd.none
    | CollectionChanged collection -> 
        { model with SelectedCollections = updateSelectedCollections collection }, Cmd.none
    | TimeZoneChanged isLocal ->
        { model with TimeZone = if isLocal then TimeZone.Local else TimeZone.Utc }, Cmd.none
    | StartSearch ->
        let fetchUrl = getSearchUrl model
        startSearch model fetchUrl SearchCompleted SearchError
    | SearchCompleted result ->
        match result with
        | Result.Ok events ->
            let elapsed = getRunningTime model
            { model with 
                SearchStatus = Idle <| sprintf "Last search: %d ms (%d results)" (int elapsed.TotalMilliseconds) events.Length
                SearchResult = model.SearchResult |> Option.map (fun x -> { x with Events = events |> Array.toList })
             }, Cmd.none
        | Result.Error error ->
            { model with SearchStatus = Idle error }, Cmd.none
    | SearchError exn -> 
        { model with SearchStatus = Idle <| getErrorMessage exn }, Cmd.none
    | EventSelected index ->
        { model with SelectedEventIndex = Some index }, Cmd.ofMsg (LoadContent index)
    | EventUnselected ->
        { model with SelectedEventIndex = None; Content = "" }, Cmd.none
    | LoadContent index ->
        let fetchUrl = getContentUrl model model.SearchResult.Value.Events.[index]
        loadContent model fetchUrl ContentLoaded LoadError
    | ContentLoaded result ->
        match result with
        | Result.Ok eventContent ->
            let elapsed = getRunningTime model
            { model with 
                SearchStatus = Idle <| sprintf "Last search: %d ms" (int elapsed.TotalMilliseconds)
                Content = extractContent eventContent
             }, Cmd.none
        | Result.Error error ->
            { model with SearchStatus = Idle error }, Cmd.none
    | LoadError exn -> 
        { model with SearchStatus = Idle <| getErrorMessage exn }, Cmd.none
