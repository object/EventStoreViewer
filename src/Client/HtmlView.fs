module EventStoreViewer.HtmlView

open System
open Fable.React
open Fable.React.Props

open Types
open Logic

let showButton name color disabled msg dispatch =
  button [
    Disabled disabled
    OnClick (fun _ -> dispatch msg)
  ] [str name ]

let showSpace () =
  div [ClassName "control"] []

let showSearchModes searchMode dispatch =
  div [] [
      showButton "Interval" "" false SearchModeByInterval dispatch
      showSpace ()
      showButton "Id/Program" "" false SearchModeByIdOrProgram dispatch ]

let showCollection collection model dispatch =
  button [
    OnClick (fun _ -> dispatch <| CollectionChanged collection)
  ] [ str <| getCollectionDisplayName collection ]

let showIdOrProgramInput (model : Model) (dispatch : Msg -> unit) =
  div [] [
    div [ClassName "control"] [input [
      Disabled (model.SearchMode <> ByIdOrProgram)
      OnChange (fun evt -> dispatch (ProgramIdChanged evt.Value))
    ]]
  ]

let showSearchIntervals (intervals : string array) modifiers (model : Model) dispatch =
  let disabled = model.SearchMode = ByIdOrProgram
  div [] [
    showButton intervals.[0] "" disabled SearchIntervalOneHour dispatch
    showSpace ()
    showButton intervals.[1] "" disabled SearchIntervalFourHours dispatch
    showSpace ()
    showButton intervals.[2] "" disabled SearchIntervalOneDay dispatch ]

let showIntervalTime text isValid changeMsg model dispatch =
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
  let disabled = model.SearchMode = ByIdOrProgram
  div [] [
    showButton "Custom interval" "" disabled SearchIntervalCustom dispatch
    showSpace ()
    showTimezoneMode model dispatch ]

let showIntervalRange (model : Model) dispatch =
  div [] [
      showIntervalTime model.FromTime Model.hasValidFromTime FromTimeChanged model dispatch
      showSpace ()
      showIntervalTime model.ToTime Model.hasValidToTime ToTimeChanged model dispatch ]

let showSettings model dispatch =
  div [] [
    showSearchModes model.SearchMode dispatch
    showIdOrProgramInput model dispatch
    showSearchIntervals [|"1 hour"; "4 hours"; "24 hours"|] [] model dispatch
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
      div [] [showCollection x.EventSource model dispatch]))

let showStatus (model : Model) dispatch =
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

let showResultRow (eventRow : EventRow) i isSmallScreen (model : Model) dispatch =
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

let showResultRows isSmallScreen (model : Model) dispatch =
  match model.SearchResult with
  | Some result ->
      result.Events |> List.mapi (fun i x -> 
        let eventRow = createResultRow result x model
        showResultRow eventRow i isSmallScreen model dispatch)
    | None -> []

let showResults (model : Model) dispatch =
  div [] [
    table []
      [ tbody [] (showResultRows false model dispatch) ]
  ]

let showContent (model : Model) dispatch =
  div []
    [ p [] [ str model.Content ] 
  ]

let view model dispatch =
  div [] [
    showControls model dispatch
    showResults model dispatch
    showContent model dispatch
  ]
