module EventStoreViewer.App

open Elmish
open Elmish.React
open Elmish.Debug
open Elmish.HMR

open Update

Program.mkProgram init update HtmlView.view
#if DEBUG
|> Program.withDebugger
#endif
|> Program.withReactBatched "elmish-app"
|> Program.run
