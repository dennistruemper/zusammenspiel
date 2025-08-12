port module LocalStorage exposing (fromJS, toJS)

-- OUTGOING PORT


port toJS : String -> Cmd msg



-- INCOMING PORT


port fromJS : (String -> msg) -> Sub msg
