module EventStoreViewer.Messages

open Model

type Msg =
    | LoadAppSettings
    | AppSettingsLoaded of Result<AppSettings,string>
    | AppSettingsError of exn
    | SearchModeById
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
