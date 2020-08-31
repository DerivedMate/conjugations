module Pie exposing (..)

import Array
import Data exposing (..)
import Svg exposing (Svg, a, desc, g, path, svg, text, title)
import Svg.Attributes exposing (attributeName, d, fill, style, title, viewBox, xlinkHref)
import Url exposing (toString)


type alias PrePie =
    { x : Int
    , url : String
    }


type alias PieAble =
    { percent : Float
    , url : String
    }


pieify : List PrePie -> List PieAble
pieify ps =
    let
        total =
            List.foldl (\{ x } a -> a + x) 0 ps |> toFloat

        pas =
            List.map (\{ url, x } -> { url = url, percent = toFloat x / total }) ps
    in
    pas


largeArcFlag : Float -> Int
largeArcFlag p =
    if p > 0.5 then
        1

    else
        0


polarish : Float -> ( Float, Float )
polarish pr =
    ( cos (2 * pi * pr), sin (2 * pi * pr) )


colors : Array.Array String
colors =
    [ "#21292f"
    , "#43646a"
    , "#cebca3"
    , "#adfbf5"
    , "#b0af4e"
    ]
        |> Array.fromList


getColor : Int -> String
getColor i =
    Array.get (modBy (Array.length colors) i) colors |> Maybe.withDefault "red"


arcify : { lx : Float, ly : Float, accPr : Float, i : Int } -> List PieAble -> List (Svg msg) -> List (Svg msg)
arcify { lx, ly, accPr, i } ps gs =
    case ps of
        [] ->
            List.reverse gs

        p :: ps_ ->
            let
                accPr_ =
                    accPr + p.percent

                ( lx_, ly_ ) =
                    polarish accPr_

                laf =
                    largeArcFlag p.percent

                -- Path params
                m =
                    "M " ++ String.fromFloat lx ++ " " ++ String.fromFloat ly

                l =
                    "L 0 0"

                a_ =
                    "A 1 1 0 " ++ String.fromInt laf ++ " 1 " ++ String.fromFloat lx_ ++ " " ++ String.fromFloat ly_

                lable =
                    String.fromFloat ((p.percent * 1.0e4 |> floor |> toFloat) / 100) ++ "%"

                -- path
                g_ =
                    a [ xlinkHref p.url ]
                        [ g []
                            [ Svg.title [] [ text lable ]
                            , path [ d (String.join " " [ m, a_, l ]), fill (getColor i) ] []
                            ]
                        ]
            in
            arcify { lx = lx_, ly = ly_, accPr = accPr_, i = i + 1 } ps_ (g_ :: gs)


pie : List PieAble -> Svg msg
pie ps =
    svg [ viewBox "-1 -1 2 2", style "transform: rotate(-0.25turn)" ] (arcify { lx = 1.0, ly = 0.0, accPr = 0.0, i = 0 } ps [])
