module EventStoreViewer.HtmlViewWithComponents

open System
open Fable.React
open Fable.React.Props

open Model
open Messages
open ViewUtils

let inline prettifyJson json : ReactElement =
  ofImport "default" "react-json-view"
    {|
      src = json
      enableClipboard = false
      displayObjectSize = false
      displayDataTypes = false
    |}
    []

let inline prettifyXml xml : ReactElement =
  ofImport "default" "react-xml-viewer"
    {| xml = xml |} []

let showButton name disabled msg dispatch =
  button [
    Disabled disabled
    OnClick (fun _ -> dispatch msg)
  ] [str name ]

let showSpace () =
  div [ClassName "control"] []

let showSearchModes dispatch =
  div [] [
      showButton "Interval" false SearchModeByInterval dispatch
      showSpace ()
      showButton "Id" false SearchModeById dispatch ]

let showCollection collection dispatch =
  button [
    OnClick (fun _ -> dispatch <| CollectionChanged collection)
  ] [ str <| getCollectionDisplayName collection ]

let showIdInput (model : Model) (dispatch : Msg -> unit) =
  div [] [
    div [ClassName "control"] [input [
      Disabled (model.SearchMode <> ById)
      OnChange (fun evt -> dispatch (ProgramIdChanged evt.Value))
    ]]
  ]

let showSearchIntervals (intervals : string array) (model : Model) dispatch =
  let disabled = model.SearchMode = ById
  div [] [
    showButton intervals.[0] disabled SearchIntervalOneHour dispatch
    showSpace ()
    showButton intervals.[1] disabled SearchIntervalFourHours dispatch
    showSpace ()
    showButton intervals.[2] disabled SearchIntervalOneDay dispatch ]

let showIntervalTime changeMsg model dispatch =
  input [
    Disabled (model.SearchMode <> ByInterval || model.SearchInterval <> Custom)
    OnChange (fun evt -> dispatch (changeMsg evt.Value))
  ]

let showTimezoneMode (model : Model) dispatch =
  div [] [
    input [
      Props.Type "checkbox"
      Disabled (model.SearchMode <> ByInterval)
      Checked (model.TimeZone = TimeZone.Local)
      OnChange (fun evt -> dispatch (TimeZoneChanged evt.Checked))
    ] 
    label [] [str "Local"]
  ]

let showCustomInterval (model : Model) dispatch =
  let disabled = model.SearchMode = ById
  div [] [
    showButton "Custom interval" disabled SearchIntervalCustom dispatch
    showSpace ()
    showTimezoneMode model dispatch ]

let showIntervalRange (model : Model) dispatch =
  div [] [
      showIntervalTime FromTimeChanged model dispatch
      showSpace ()
      showIntervalTime ToTimeChanged model dispatch ]

let showSettings model dispatch =
  div [] [
    showSearchModes dispatch
    showIdInput model dispatch
    showSearchIntervals [|"1 hour"; "4 hours"; "24 hours"|] model dispatch
    showCustomInterval model dispatch
    showIntervalRange model dispatch
  ]

let showSearchButton (model : Model) dispatch =
  div [] [
    button [
      Disabled <| not (Model.isValid model)
      OnClick (fun _ -> dispatch StartSearch)
      ] [str "Search"]
  ]

let showCollections model dispatch =
  div [] 
    (Collections.all |> List.map (fun x -> 
      div [] [showCollection x.EventSource dispatch]))

let showStatus (model : Model) _dispatch =
  let (status, text) = 
    match model.SearchStatus with
    | Idle text -> ("Idle", text)
    | Running (text, _) -> ("Running", text)
  div [] [
    div [ ]
        [ p [ ] [ str <| sprintf "Status: %s" status]
          p [ ] [ str text ] ] ]

let showControls model dispatch =
  div [] [
    showSettings model dispatch
    showSearchButton model dispatch
    showCollections model dispatch
    showStatus model dispatch
  ]

let showResultRow (eventRow : EventRow) i (model : Model) dispatch =
  let clickHandler = 
    match model.SelectedEventIndex with 
    | Some idx when idx = i -> EventUnselected
    | _ -> EventSelected i
  let columns = [
      td [] [span [] [str eventRow.CollectionName]]
      td [] [span [] [str eventRow.EventId]]
      td [] [span [] [str eventRow.Description]]
      td [] [span [] [str <| eventRow.Date]]
      td [] [span [] [str <| eventRow.Time] ]]
  tr [OnClick (fun _ -> dispatch clickHandler)] columns

let showResultRows (model : Model) dispatch =
  match model.SearchResult with
  | Some result ->
      result.Events |> List.mapi (fun i x -> 
        let eventRow = createResultRow result x model
        showResultRow eventRow i model dispatch)
    | None -> []

let showResults (model : Model) dispatch =
  div [] [
    table []
      [ tbody [] (showResultRows model dispatch) ]
  ]

let showContent (model : Model) _dispatch =
  let content = 
    match model.ContentType with
    | ContentType.None -> str ""
    | ContentType.Json -> prettifyJson model.Content
    | ContentType.Xml -> prettifyXml model.ContentAsString
  div []
    [ content ] 

let view model dispatch =
  div [] [
    showControls model dispatch
    showResults model dispatch
    showContent model dispatch
  ]
