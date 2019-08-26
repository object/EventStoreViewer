module EventStoreViewer.Messages

open Model

type Msg =
    // AppSettings are loaded on start
    | LoadAppSettings
    | AppSettingsLoaded of Result<AppSettings,string>
    | AppSettingsError of exn

    // Search options
    | SearchModeById
    | SearchModeByInterval
    | SearchIntervalOneHour
    | SearchIntervalFourHours
    | SearchIntervalOneDay
    | SearchIntervalCustom

    // Controls change handlers
    | ProgramIdChanged of string
    | FromTimeChanged of string
    | ToTimeChanged of string
    | CollectionChanged of EventSource
    | TimeZoneChanged of bool
    | EventSelected of int
    | EventUnselected

    // Search result handlers 
    | StartSearch
    | SearchCompleted of Result<ServiceEvent array,string>
    | SearchError of exn

    // Event content load handlers
    | LoadContent of int
    | ContentLoaded of  Result<EventContent,string>
    | LoadError of exn
