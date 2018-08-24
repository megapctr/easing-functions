module Test exposing (main)

import Ease exposing (..)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttributes


width =
    120


height =
    64


plot : Int -> ( Easing, String ) -> Svg a
plot i ( f, name ) =
    Svg.g
        [ SvgAttributes.transform
            ("translate("
                ++ String.fromInt ((width + 10) * modBy 6 i)
                ++ ","
                ++ String.fromInt ((height + 50) * (i // 6))
                ++ ")"
            )
        ]
        [ Svg.rect
            [ SvgAttributes.width (String.fromInt width)
            , SvgAttributes.height (String.fromInt 1)
            , SvgAttributes.fill "gray"
            ]
            []
        , Svg.rect
            [ SvgAttributes.width (String.fromInt width)
            , SvgAttributes.height (String.fromInt 1)
            , SvgAttributes.y (String.fromInt height)
            , SvgAttributes.fill "gray"
            ]
            []
        , Svg.path
            [ List.map
                (toFloat
                    >> (\x ->
                            ( x
                            , height - f (x / width) * height
                            )
                       )
                )
                (List.range 0 (round width))
                |> path
                |> SvgAttributes.d
            , SvgAttributes.stroke "black"
            , SvgAttributes.strokeWidth "2"
            , SvgAttributes.fill "transparent"
            ]
            []
        , Svg.text_
            [ SvgAttributes.y (String.fromInt -15)
            , SvgAttributes.x (String.fromInt 30)
            , SvgAttributes.style "font: 14px sans-serif;"
            ]
            [ Svg.text name ]
        ]


path : List ( Float, Float ) -> String
path list =
    case list of
        ( x, y ) :: tail ->
            "M"
                ++ String.fromFloat x
                ++ ","
                ++ String.fromFloat y
                ++ String.join ""
                    (List.map
                        (\( a, b ) ->
                            "L"
                                ++ String.fromFloat a
                                ++ ","
                                ++ String.fromFloat b
                        )
                        tail
                    )

        _ ->
            ""


title : Svg a
title =
    Svg.text_
        [ SvgAttributes.y "-60"
        , SvgAttributes.x "80"
        , SvgAttributes.style "font: 14px sans-serif;"
        ]
        [ Svg.text "This is a replication of easings.net for testing purposes. You can see the plots are nearly identical." ]


main : Svg a
main =
    Svg.svg
        [ SvgAttributes.width "850"
        , SvgAttributes.height "650"
        , SvgAttributes.viewBox "-10 -60 840 590"
        , SvgAttributes.style "display:block;margin:auto;"
        ]
        (title :: List.indexedMap plot easingFunctions)


easingFunctions =
    [ ( inSine, "inSine" )
    , ( outSine, "outSine" )
    , ( inOutSine, "inOutSine" )
    , ( inQuad, "inQuad" )
    , ( outQuad, "outQuad" )
    , ( inOutQuad, "inOutQuad" )
    , ( inCubic, "inCubic" )
    , ( outCubic, "outCubic" )
    , ( inOutCubic, "inOutCubic" )
    , ( inQuart, "inQuart" )
    , ( outQuart, "outQuart" )
    , ( inOutQuart, "inOutQuart" )
    , ( inQuint, "inQuint" )
    , ( outQuint, "outQuint" )
    , ( inOutQuint, "inOutQuint" )
    , ( inExpo, "inExpo" )
    , ( outExpo, "outExpo" )
    , ( inOutExpo, "inOutExpo" )
    , ( inCirc, "inCirc" )
    , ( outCirc, "outCirc" )
    , ( inOutCirc, "inOutCirc" )
    , ( inBack, "inBack" )
    , ( outBack, "outBack" )
    , ( inOutBack, "inOutBack" )
    , ( inElastic, "inElastic" )
    , ( outElastic, "outElastic" )
    , ( inOutElastic, "inOutElastic" )
    , ( inBounce, "inBounce" )
    , ( outBounce, "outBounce" )
    , ( inOutBounce, "inOutBounce" )
    ]
