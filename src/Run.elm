port module Run exposing (once)

import Platform
import Json.Decode


port printAndExit : String -> Cmd msh

type alias Flags =
    List String

once : (Flags -> String) -> Program Flags String msg
once run =
    Platform.programWithFlags
        { init = \flags -> ( "", printAndExit (run flags) )
        , update = \msg model -> ( model, Cmd.none )
        , subscriptions = \model -> Sub.none
        }
