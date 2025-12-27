module Evergreen.V9.IcsParser exposing (..)


type alias ParsedMatch =
    { opponent : String
    , date : String
    , time : String
    , venue : String
    , isHome : Bool
    }
