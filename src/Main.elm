module Main exposing (main)

import Html
import Color exposing (..)
import Style exposing (style)
import Element exposing (textLayout, layout, row, column, text, underline, link, el, paragraph, image)
import Element.Attributes exposing (spacing, padding, paddingBottom, paddingTop, center, verticalCenter, height, width, maxWidth, px, fill)
import Style.Scale
import Style.Color as Color
import Style.Font as Font
import Style.Background as Background
import Style.Shadow as Shadow


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    Int


init : ( Model, Cmd msg )
init =
    ( 0, Cmd.none )


type Msg
    = Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type Styles
    = NoStyle
    | Placeholder
    | Page
    | Header
    | Title
    | ContestBanner
    | ActionCall
    | ActionTagline
    | ActionList
    | AsSeenIn
    | Qualifications
    | Qualifier
    | Question
    | Answer


stylesheet : Style.StyleSheet Styles variation
stylesheet =
    Style.styleSheet
        [ style NoStyle []
        , style Placeholder
            [ Font.size (fontScale 2)
            ]
        , style Page
            [ Font.typeface (fontStack SansSerif) ]
        , style Header
            [ Color.background (Color.rgb 78 64 169)
            , Color.text white
            ]
        , style ContestBanner
            [ Color.background (Color.rgb 46 255 217)
            , Color.text (Color.rgb 41 47 54)
            , Font.uppercase
            , Font.size (fontScale 1)
            , Font.weight 700
            , Shadow.box { offset = ( 0, 2 ), size = 2, blur = 0, color = Color.rgba 41 47 54 0.24 }
            ]
        , style Title
            [ Font.size (fontScale 3)
            ]
        , style ActionCall
            [ Background.gradient (pi * 3 / 4) [ Background.step (Color.rgb 71 161 255), Background.step (Color.rgb 99 120 253) ] ]
        , style ActionTagline
            [ Color.text white
            , Font.size (fontScale 3)
            , Font.weight 700
            , Shadow.text { offset = ( 0, 2 ), blur = 0, color = Color.rgba 0 0 0 0.2 }
            ]
        , style ActionList
            [ Color.text (Color.rgb 46 255 217)
            , Font.size (fontScale 2)
            , Font.weight 700
            ]
        , style AsSeenIn
            [ Color.background white
            , Font.size (fontScale 2)
            , Color.text (Color.rgb 78 72 72)
            ]
        , style Qualifications []
        , style Qualifier []
        , style Question
            [ Font.bold ]
        , style Answer []
        ]


fontScale : Int -> Float
fontScale =
    Style.Scale.roundedModular 18 1.618


type FontStack
    = SansSerif
    | Serif
    | Mono


fontStack : FontStack -> List Style.Font
fontStack stack =
    case stack of
        SansSerif ->
            List.map Font.font [ "Helvetica Neue", "Helvetica", "Arial", "sans-serif" ]

        Serif ->
            List.map Font.font [ "Garamond", "serif" ]

        Mono ->
            List.map Font.font [ "Inconsolata", "monospace" ]


email : String
email =
    "mailto:crockagile@gmail.com"


view : Model -> Html.Html Msg
view model =
    Element.layout stylesheet <|
        column Page
            [ center, width fill ]
            [ header
            , actionCall
            , asSeenIn
            , qualifications
            ]


header : Element.Element Styles variation Msg
header =
    Element.header Header
        []
        (row NoStyle
            [ center, verticalCenter, width fill, height (px 85) ]
            [ Element.h1 Title [] (text "Jeff Wants Anchor")
            ]
        )


contestBanner : Element.Element Styles variation Msg
contestBanner =
    el ContestBanner
        [ width fill, center ]
        (link email <|
            (el NoStyle [ padding 6, center ] (text "Hire Jeff Crocker.  Find out how💰"))
        )


actionCall : Element.Element Styles variation Msg
actionCall =
    column ActionCall
        [ width fill, center, spacing 40, paddingBottom 60 ]
        [ contestBanner
        , Element.h2 ActionTagline [] (text "Writes code, right from your office.")
        , Element.h3 ActionList [] (text "Get products, cooperation, and passion, all for one salary.")
        , Element.paragraph ActionList
            []
            [ el NoStyle [ padding 6 ] (text "Already have engineers?")
            , link email <| (underline "Great, I work better in teams.")
            ]
        ]


asSeenIn : Element.Element Styles variation Msg
asSeenIn =
    column AsSeenIn
        [ spacing 20, center, paddingTop 25 ]
        [ Element.h4 NoStyle
            []
            (text "As seen in...")
        , Element.wrappedRow
            NoStyle
            [ width fill, spacing 40 ]
            (List.map
                asSeenInLogo
                [ ( "polysync.png", "PolySync" ), ( "souperseconds.png", "Souper Seconds" ), ( "clearwater.png", "Clearwater Analytics" ) ]
            )
        ]


asSeenInLogo : ( String, String ) -> Element.Element Styles variation Msg
asSeenInLogo ( src, company ) =
    image NoStyle [ height (px 100) ] { src = src, caption = company ++ " Company Logo" }


qualifications : Element.Element Styles variation Msg
qualifications =
    Element.textLayout Qualifications
        [ width fill, height fill ]
        [ (q_and_a "Qualified?" "Does it reload?")
        ]


q_and_a : String -> String -> Element.Element Styles variation Msg
q_and_a question answer =
    paragraph Qualifier
        []
        [ el Question [ padding 15 ] (text question)
        , el Answer [] (text answer)
        ]
