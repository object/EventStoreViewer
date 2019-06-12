module EventStoreViewer.App

open Elmish
open Elmish.React
open Elmish.Debug
open Elmish.HMR

open State

Program.mkProgram init update FulmaView.view
#if DEBUG
|> Program.withDebugger
#endif
|> Program.withReactBatched "elmish-app"
|> Program.run
