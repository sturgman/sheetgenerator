
import Html exposing (..)
import Html.Attributes exposing (..)
import Svg
import Svg.Attributes as Svga
import Html.App as App
import Html.Events exposing (onClick, onInput)
import String
import List.Extra
import List

main =
    App.beginnerProgram { model = model, view = view, update = update }


-- Model

type alias Model =
    { numxatoms : Int
    , numyatoms : Int
-- The reason I include these here instead of just renering them is that eventually I would like my user to export a file based on these lists.
    , listatoms : List Atom
    , listbonds : List Bond
    , valid : Bool
    }

model : Model
model =
    { numxatoms = 1
    , numyatoms = 1
    , listatoms = []
    , listbonds = []
    , valid = True
    } 


-- Supporting types
    
type alias Atom =
    { atype : Int
    , x : Int
    , y : Int
    } 
                
type alias Bond =
    { btype : Int
    , at1 : Int
    , at2 : Int
    }


type Msg = Changex String| Changey String | Generate

update : Msg -> Model -> Model
update msg model =
    case msg of
        -- I am converting strings to ints here...
        -- It took me a long time to realize how to do this.
        -- In particular: nested case statements! And general use of Result.
        Changex str -> case String.toInt str of
                           Ok numx ->
                               if (numx < 50 && model.numyatoms < 50) then {model | numxatoms = numx, valid = True} else {model | numxatoms = numx, valid = False}
                           Err err -> {model | valid = False}
        Changey str -> case String.toInt str of
                           Ok numy -> if (numy < 50 && model.numxatoms < 50) then {model | numyatoms = numy , valid = True} else {model | numyatoms = numy, valid = False}
                           Err err -> {model | valid = False}
        Generate -> if model.valid == True then makeSheet model else model

makeSheet : Model -> Model
makeSheet model =
    model
        |> makeAtoms
-- once bond are implemented |> makeBonds

makeAtoms : Model -> Model
makeAtoms model =
    let
        x = [1..model.numxatoms]
        y = [1..model.numyatoms]
        makeAtom : Int -> (Int,Int) -> Atom
        makeAtom t coord =
            let
                (xn,yn)=coord
            in
                {atype=t,x=xn,y=yn}

        -- This is for future functionality
        makeAtomT = makeAtom 1
        listcoords=List.Extra.lift2 (,) x y
    in
        {model | listatoms= List.map makeAtomT listcoords}

-- What is the type annotation for this? I have no idea. This particular snippet was borrowed from stack overflow. I THINK I understand what is happening
renderList lst =
    lst
    |> List.map (\l -> li [] [ text ((toString l.x) ++ "  " ++ (toString l.y)) ])
    |> ul []


-- I mirrored the list rendering function to an svg version.
renderSvg lst =
    let
        renderCircle atm =
            Svg.circle [ Svga.cx ((toString atm.x) ++ "%")
                       , Svga.cy ((toString atm.y) ++ "%")
                       , Svga.r "0.25%"] []
    in
        Svg.svg [ Svga.width "100%"
                , Svga.height "100%"
                , Svga.viewBox "0 0 1000 1000"
                ] (List.map renderCircle lst)

view : Model -> Html Msg
view model =
    div []
        [ div [] [text "This is work in progress! A tool to write coordiantes and bonds for a sheet. Input numbers less than 50."]
        , input [ placeholder "Number of atoms in x direction", onInput Changex ] []
        , input [ placeholder "Number of atoms in y direction", onInput Changey ] []
        ,button [ onClick Generate ] [ Html.text "Generate Sheet" ]
        , div [] [text ("Model is " ++ (if model.valid then "ready" else "too big, disabled for now."))]
        , div [] [renderSvg model.listatoms]
-- only for debugging       , div [] [renderList model.listatoms]
        ]
