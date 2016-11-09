
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

type alias Model a =
    { numxatoms : Int
    , numyatoms : Int
-- The reason I include these here instead of just renering them is that eventually I would like my user to export a file based on these lists.
    , listatoms : List Atom
    , listbonds : List Bond
    , image : Svg.Svg a
    , valid : Bool
    }

model : Model a
model =
    { numxatoms = 1
    , numyatoms = 1
    , listatoms = []
    , listbonds = []
    , image = Svg.svg [] []
    , valid = True
    } 


-- Supporting types
    
type alias Atom =
    { atype : Int
    , anum : Int
    , x : Int
    , y : Int
    } 
                
type alias Bond =
    { btype : Int
    , at1 : Int
    , at2 : Int
    }


type Msg
    = Changex String
    | Changey String
    | Generate


update : Msg -> Model a -> Model a
update msg model =
    case msg of
        -- I am converting strings to ints here...
        -- It took me a long time to realize how to do this.
        -- In particular: nested case statements! And general use of Result.
        Changex str ->
            case String.toInt str of
                Ok numx ->
                    if (numx < 50 && model.numyatoms < 50) then
                        {model | numxatoms = numx, valid = True}

                    else
                        {model | numxatoms = numx, valid = False}
                            
                Err err ->
                    {model | valid = False}

        Changey str ->
            case String.toInt str of
                Ok numy ->
                    if (numy < 50 && model.numxatoms < 50) then
                        {model | numyatoms = numy , valid = True}
                            
                    else
                        {model | numyatoms = numy, valid = False}

                Err err ->
                    {model | valid = False}

        Generate ->
            if model.valid == True then
                makeSheet model

            else model

                
makeSheet : Model a -> Model a
makeSheet model =
    model
        |> makeAtoms
        |> makeBonds
        |> renderSvg


makeAtoms : Model a -> Model a
makeAtoms model =
    let
        x =
            [1..model.numxatoms]

        y =
            [1..model.numyatoms]

        makeAtom : Int -> Int -> (Int,Int) -> Atom
        makeAtom t idx coord =
            let
                (yn,xn)=coord
            in
                {atype=t,anum=idx,x=xn,y=yn}

        -- This is for future functionality
        makeAtomT = makeAtom 1
        listcoords=List.Extra.lift2 (,) y x
    in
        {model | listatoms= List.indexedMap makeAtomT listcoords}


makeBonds : Model a -> Model a
makeBonds model =
    let
        makeBond : Int -> Atom -> List Bond
        makeBond idx atom =
            if (atomRegular atom model) then
                List.append (makebondright idx) (makebonddown idx model)
                    
            else if (atomFinalColumn atom model) then
                makebonddown idx model
                
            else if (atomFinalRow atom model) then
                makebondright idx

            else 
                []

    in
        {model | listbonds = List.concat (List.indexedMap makeBond model.listatoms)}

             
makebondright : Int -> List Bond
makebondright id =
    [
     { btype = 1
     , at1 = id
     , at2 = id + 1
     }
    ]


makebonddown : Int -> Model a -> List Bond
makebonddown id model =
    [
     { btype = 1
     , at1 = id
     , at2 = id + model.numxatoms
     }
    ]

atomRegular : Atom -> Model a -> Bool
atomRegular atom model =
    if atom.x < model.numxatoms && atom.y < model.numyatoms then True else False

atomFinalColumn : Atom -> Model a -> Bool
atomFinalColumn atom model =
    if atom.x == model.numxatoms && atom.y < model.numyatoms then True else False

atomFinalRow : Atom -> Model a -> Bool
atomFinalRow atom model =
    if atom.x < model.numxatoms && atom.y == model.numyatoms then True else False


-- What is the type annotation for this? I have no idea. This particular snippet was borrowed from stack overflow. I THINK I understand what is happening
renderListAtoms lst =
    lst
    |> List.map (\l -> li [] [ text
                               ((toString l.x) ++ "  " ++ (toString l.y)) ])
    |> ul []


renderListBonds lst =
    lst
    |> List.map (\l -> li [] [ text
                               ((toString (l.at1+1)) ++ "  " ++ (toString (l.at2+1))) ])
    |> ul []


-- I mirrored the list rendering function to an svg version.
renderSvg model =
    let
        atomlist = model.listatoms

        toPercent value =
            (toString value)
        
        renderCircle atm =
            Svg.circle [ Svga.cx (toPercent atm.x)
                       , Svga.cy (toPercent atm.y) 
                       , Svga.r "0.25"] []
                    
        renderBond atom =
            let
                swidth = "0.1"
                bcolor = "red"
                        
            in 
            if (atomRegular atom model) then
                [
                 (Svg.line [ Svga.x1 (toPercent atom.x) 
                           , Svga.y1 (toPercent atom.y)
                           , Svga.x2 (toPercent (atom.x + 1))
                           , Svga.y2 (toPercent (atom.y))
                           , Svga.strokeWidth swidth
                           , Svga.stroke bcolor
                           ] [])
                     
                ,(Svg.line [ Svga.x1 (toPercent atom.x)
                           , Svga.y1 (toPercent atom.y)
                           , Svga.x2 (toPercent (atom.x))
                           , Svga.y2 (toPercent (atom.y + 1))
                           , Svga.strokeWidth swidth
                           , Svga.stroke bcolor
                           ] [])
                ]

                    
            else if (atomFinalColumn atom model) then
                     [
                      (Svg.line [ Svga.x1 (toPercent atom.x)
                                , Svga.y1 (toPercent atom.y)
                                , Svga.x2 (toPercent (atom.x))
                                , Svga.y2 (toPercent (atom.y + 1))
                                , Svga.strokeWidth swidth
                                , Svga.stroke bcolor
                                ] [])
                     ]
                
                 else if (atomFinalRow atom model) then
                     [
                      (Svg.line [ Svga.x1 (toPercent atom.x)
                                , Svga.y1 (toPercent atom.y)
                                , Svga.x2 (toPercent (atom.x + 1))
                                , Svga.y2 (toPercent (atom.y))
                                , Svga.strokeWidth swidth
                                , Svga.stroke bcolor
                                ] [])
                     ]


            else 
                []
    
    in
        {model |
             image = Svg.svg [ Svga.width (toString (model.numxatoms*20))
                             , Svga.height "auto"
                             , Svga.viewBox ("0 0 " ++ (toString model.numxatoms) ++ " " ++ (toString model.numyatoms))
                             ] [Svg.g [Svga.transform "translate(-0.5,-0.5)"] (List.append (List.concat (List.map renderBond atomlist)) (List.map renderCircle atomlist))]
        }

-- I dont understand this... why does the anotation have to be this?            
view : Model Msg -> Html Msg
view model =
    div []
        [ div [] [text "This is work in progress! A tool to write coordinates
                        and bonds for a sheet. Input numbers less than 50."]
        , input [ placeholder "Number of atoms in x direction", onInput Changex ] []
        , input [ placeholder "Number of atoms in y direction", onInput Changey ] []
        ,button [ onClick Generate ] [ Html.text "Generate Sheet" ]
        , div [] [text
                  ("Model is " ++ (if model.valid then
                                       "ready"
                                   else
                                       "too big, disabled for now."))]
        , div [] [model.image]
        , div [] [h2 [] [text "Atom locations"]]
        , div [] [renderListAtoms model.listatoms]
        , div [] [h2 [] [text "Bonds between atoms"]]
        , div [] [renderListBonds model.listbonds]
        ]
