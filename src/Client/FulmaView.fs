module EventStoreViewer.FulmaView

open System
open Fable.React
open Fable.React.Props

open Fulma

open Model
open Messages
open ViewUtils

let showButton name color disabled msg dispatch =
    Button.button [ 
      Button.IsFullWidth
      Button.Color color
      Button.Disabled disabled
      Button.OnClick (fun _ -> dispatch msg) ] 
      [str name ]

let showSpace () =
  Control.div [] []

let showSearchModes searchMode dispatch =
  let programColor, intervalColor = 
    match searchMode with 
    | ById -> (Color.IsPrimary, Color.IsLight)
    | ByInterval -> (Color.IsLight, Color.IsPrimary)
  Field.div [Field.IsGrouped] [
      showButton "Interval" intervalColor false SearchModeByInterval dispatch
      showSpace ()
      showButton "Id" programColor false SearchModeById dispatch ]

let showCollection collection model dispatch =
  let color = 
    if model.SelectedCollections |> List.contains collection 
    then Color.IsPrimary 
    else Color.IsLight
  Button.button [ 
    Button.IsFullWidth
    Button.Color color 
    Button.OnClick (fun _ -> dispatch <| CollectionChanged collection) ] 
    [ str <| getCollectionDisplayName collection ]

let showIdInput model dispatch =
  let inputColor = if Model.hasValidId model then Color.IsSuccess else Color.IsDanger
  let textColor = if model.SearchMode = ById then Color.IsBlack else Color.IsLight
  Field.div [] [
    Control.div [] [
      Input.text
        [ Input.Color inputColor
          Input.Modifiers [Modifier.TextColor textColor]
          Input.Disabled (model.SearchMode <> ById)
          Input.OnChange (fun evt -> dispatch (ProgramIdChanged evt.Value))
          Input.DefaultValue model.Id
          Input.Placeholder "Id" ] ] ]

let showSearchIntervals (intervals : string array) modifiers (model : Model) dispatch =
  let getColor buttonInterval = 
    match model.SearchMode with 
    | ById -> Color.IsLight
    | ByInterval when model.SearchInterval = buttonInterval -> Color.IsPrimary
    | _ -> Color.IsLight
  let disabled = model.SearchMode = ById
  Field.div [Field.IsGrouped; Field.Modifiers modifiers] [
    showButton intervals.[0] (getColor OneHour) disabled SearchIntervalOneHour dispatch
    showSpace ()
    showButton intervals.[1] (getColor FourHours) disabled SearchIntervalFourHours dispatch
    showSpace ()
    showButton intervals.[2] (getColor OneDay) disabled SearchIntervalOneDay dispatch ]

let showIntervalTime text placeholderText isValid changeMsg model dispatch =
  let inputColor = if isValid model then Color.IsSuccess else Color.IsDanger
  let textColor = if model.SearchMode = ByInterval && model.SearchInterval = Custom then Color.IsBlack else Color.IsLight
  Input.text [ 
    Input.Modifiers [Modifier.TextColor textColor]
    Input.Color inputColor
    Input.Disabled (model.SearchMode <> ByInterval || model.SearchInterval <> Custom)
    Input.OnChange (fun evt -> dispatch (changeMsg evt.Value))
    Input.DefaultValue text
    Input.Placeholder placeholderText ]

let showTimezoneMode (model : Model) dispatch =
  Control.div []
    [ div [ Style [LineHeight "36px"] ] [
      Checkbox.checkbox [] [
        Checkbox.input [Props [
          Disabled (model.SearchMode <> ByInterval)
          Checked (model.TimeZone = TimeZone.Local)
          OnChange (fun evt -> dispatch (TimeZoneChanged evt.Checked))]]
        str "Local" ] ] ]

let showCustomInterval (model : Model) dispatch =
  let getColor buttonInterval = 
    match model.SearchMode with 
    | ById -> Color.IsLight
    | ByInterval when model.SearchInterval = buttonInterval -> Color.IsPrimary
    | _ -> Color.IsLight
  let disabled = model.SearchMode = ById
  Field.div [Field.IsGrouped; Field.Modifiers [Modifier.IsHidden (Screen.Touch, true)]] [
    showButton "Custom interval" (getColor Custom) disabled SearchIntervalCustom dispatch
    showSpace ()
    showTimezoneMode model dispatch ]

let showIntervalRange (model : Model) dispatch =
  let timeMode = 
    match model.TimeZone with 
    | TimeZone.Utc -> "UTC"
    | TimeZone.Local -> "local"
  Field.div [Field.IsGrouped; Field.Modifiers [Modifier.IsHidden (Screen.Touch, true)]] [
      showIntervalTime model.FromTime (sprintf "from %s time" timeMode) Model.hasValidFromTime FromTimeChanged model dispatch
      showSpace ()
      showIntervalTime model.ToTime (sprintf "to %s time" timeMode) Model.hasValidToTime ToTimeChanged model dispatch ]

let showSettings model dispatch =
  Column.column [] [
    showSearchModes model.SearchMode dispatch
    showIdInput model dispatch
    showSearchIntervals [|"1 hour"; "4 hours"; "24 hours"|] [Modifier.IsHidden (Screen.Touch, true)] model dispatch
    showSearchIntervals [|"1h"; "4h"; "24h"|] [Modifier.IsHidden (Screen.Desktop, true)] model dispatch
    showCustomInterval model dispatch
    showIntervalRange model dispatch
  ]

let showSearchButton (model : Model) dispatch =
  Column.column [] [
    Button.button [
      Button.Disabled <| not (Model.isValid model)
      Button.IsFullWidth
      Button.Color Color.IsInfo
      Button.OnClick (fun _ -> dispatch StartSearch) ]
      [ str "Search" ]
    ]

let showCollections model dispatch =
  Column.column [] 
    (Collections.all |> List.map (fun x -> 
      Field.div [] [showCollection x.EventSource model dispatch]))

let showStatus (model : Model) dispatch =
  let (status, text) = 
    match model.SearchStatus with
    | Idle text -> ("Idle", text)
    | Running (text, _) -> ("Running", text)
  Column.column [] [
    Content.content [ ]
        [ h5 [ ] [ str <| sprintf "Status: %s" status]
          p [ ] [ str text ] ] ]

let showControls model dispatch =
  Column.column [] [
    showSettings model dispatch
    showSearchButton model dispatch
    showCollections model dispatch
    showStatus model dispatch
  ]

let showResultRow (eventRow : EventRow) i isSmallScreen (model : Model) dispatch =
  let rowClassName, clickHandler = 
    match model.SelectedEventIndex with 
    | Some idx when idx = i -> "is-selected", EventUnselected
    | _ -> "", EventSelected i
  let modifiers = [
    Modifier.TextSize (Screen.Mobile, TextSize.Is6)
    Modifier.TextSize (Screen.Tablet, TextSize.Is7)
    Modifier.TextSize (Screen.Desktop, TextSize.Is7)
  ]
  let columns = [
      td [Props.Hidden isSmallScreen] [Text.span [Modifiers modifiers] [str eventRow.CollectionName]]
      td [] [Text.span [Modifiers ((Modifier.TextWeight TextWeight.Bold)::modifiers)] [str eventRow.EventId]]
      td [Props.Hidden isSmallScreen] [Text.span [Modifiers modifiers] [str eventRow.Description]]
      td [] [Text.span [Modifiers modifiers] [str <| eventRow.Date]]
      td [] [Text.span [Modifiers modifiers] [str <| eventRow.Time] ]]
  tr [ClassName rowClassName; OnClick (fun _ -> dispatch clickHandler)] columns

let showResultRows isSmallScreen (model : Model) dispatch =
  match model.SearchResult with
  | Some result ->
      result.Events |> List.mapi (fun i x -> 
        let eventRow = createResultRow result x model
        showResultRow eventRow i isSmallScreen model dispatch)
    | None -> []

let showResults (model : Model) dispatch =
  Column.column [] [
    Field.div [] [
      div [Style [CSSProp.OverflowY "auto"; CSSProp.Height "96vh"]] [
        Table.table [ Table.IsNarrow; Table.IsHoverable; Table.Modifiers [Modifier.IsHidden (Screen.Touch, true)] ]
          [ tbody [] (showResultRows false model dispatch) ]
        Table.table [ Table.IsNarrow; Table.IsHoverable; Table.Modifiers [Modifier.IsHidden (Screen.Desktop, true)] ]
          [ tbody [] (showResultRows true model dispatch) ]
      ]
    ]
  ]

let showContent (model : Model) dispatch =
  let modifiers = [
    Modifier.TextAlignment (Screen.All, TextAlignment.Left)
    Modifier.BackgroundColor IsLight
    Modifier.TextColor IsLink
    Modifier.TextSize (Screen.Mobile, TextSize.Is6)
    Modifier.TextSize (Screen.Tablet, TextSize.Is7)
    Modifier.TextSize (Screen.Desktop, TextSize.Is7)
  ]
  Column.column [] [
    Field.div [] [
      div [Style [WordWrap "break-word"; CSSProp.OverflowY "auto"; CSSProp.Height "96vh"]] 
        [ Content.content [Content.Modifiers modifiers ]
        [p [] [ str model.ContentAsString ] ] ] 
    ] 
  ]

let view model dispatch =
  Columns.columns [ ] [
      Column.column [ 
        Column.Width (Screen.Mobile, Column.IsFull) 
        Column.Width (Screen.Tablet, Column.IsOneThird) 
        Column.Width (Screen.Desktop, Column.IsOneQuarter) 
      ] [ showControls model dispatch ] 
      Column.column [
        Column.Width (Screen.Mobile, Column.IsFull) 
        Column.Width (Screen.Tablet, Column.IsOneThird) 
        Column.Width (Screen.Desktop, Column.IsOneQuarter) 
        Column.Width (Screen.WideScreen, Column.IsOneQuarter) 
      ] [ showResults model dispatch ]
      Column.column [
        Column.Width (Screen.Mobile, Column.IsFull) 
        Column.Width (Screen.Tablet, Column.IsOneQuarter) 
        Column.Width (Screen.Desktop, Column.IsHalf) 
        Column.Width (Screen.WideScreen, Column.IsHalf) 
      ] [ showContent model dispatch ]
  ]
