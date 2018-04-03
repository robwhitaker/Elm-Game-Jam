module Main exposing (main)

import Html

main : Program Never number msg
main =
    Html.beginnerProgram
        { model = 0
        , view = (\_ -> Html.div [] [])
        , update = always identity
        }