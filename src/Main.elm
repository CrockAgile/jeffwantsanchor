module Main exposing (main, countCharacters)

import Knapsack
import Dict
import Set
import Html
import Color exposing (..)
import Style exposing (style)
import Element.Input as Input
import Style.Scale as Scale
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Style.Background as Background
import Style.Shadow as Shadow
import Element
    exposing
        ( textLayout
        , layout
        , row
        , column
        , text
        , underline
        , link
        , el
        , paragraph
        , image
        )
import Element.Attributes
    exposing
        ( spacing
        , padding
        , paddingBottom
        , paddingTop
        , center
        , verticalCenter
        , height
        , width
        , maxWidth
        , px
        , fill
        , percent
        , spread
        , verticalSpread
        )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { challengeText : String
    , challengeLimit : Int
    }


initChallengeText : String
initChallengeText =
    "If you want to jumpstart the process of talking to us about this role, here’s a little challenge: write a program that outputs the largest unique set of characters that can be removed from this paragraph without letting its length drop below 50."


initChallengeLimit : Int
initChallengeLimit =
    50


init : ( Model, Cmd msg )
init =
    ( { challengeText = initChallengeText, challengeLimit = initChallengeLimit }, Cmd.none )


type Msg
    = NewChallengeText String
    | NewChallengeLimit String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewChallengeText challengeText ->
            ( { model | challengeText = challengeText }, Cmd.none )

        NewChallengeLimit limitString ->
            ( { model
                | challengeLimit =
                    limitString
                        |> String.filter (\c -> c /= '-')
                        |> String.toInt
                        |> Result.withDefault initChallengeLimit
              }
            , Cmd.none
            )


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
    | ShadowedWhite
    | ShadowedDark
    | ShadowedPurple
    | PurpleUnderline
    | ActionList
    | AsSeenIn
    | About
    | Challenge
    | ChallengeInput
    | ChallengeInputLimit
    | SolutionCharBox
    | SolutionCharBoxUsed


purple : Color.Color
purple =
    Color.rgb 78 64 169


teal : Color.Color
teal =
    Color.rgb 46 255 217


dark : Color.Color
dark =
    Color.rgb 41 47 54


stylesheet : Style.StyleSheet Styles Cmd
stylesheet =
    Style.styleSheet
        [ style NoStyle []
        , style Placeholder
            [ Font.size (fontScale 2)
            ]
        , style Page
            [ Font.typeface (fontStack SansSerif) ]
        , style Header
            [ Color.background purple
            , Color.text white
            ]
        , style ContestBanner
            [ Color.background teal
            , Color.text dark
            , Font.uppercase
            , Font.size (fontScale 1)
            , Font.weight 700
            , Shadow.box { offset = ( 0, 2 ), size = 2, blur = 0, color = Color.rgba 41 47 54 0.2 }
            ]
        , style Title
            [ Font.size (fontScale 3)
            ]
        , style ActionCall
            [ Background.gradient (pi * 3 / 4) [ Background.step (Color.rgb 71 161 255), Background.step (Color.rgb 99 120 253) ] ]
        , style ShadowedWhite
            [ Color.text white
            , Font.size (fontScale 3)
            , Font.weight 700
            , Shadow.text { offset = ( 0, 2 ), blur = 0, color = Color.rgba 0 0 0 0.2 }
            ]
        , style ShadowedDark
            [ Color.text dark
            , Color.decoration purple
            , Font.size (fontScale 2)
            , Font.weight 700
            , Shadow.text { offset = ( 1, 3 ), blur = 0, color = white }
            ]
        , style ShadowedPurple
            [ Color.text purple
            , Font.size (fontScale 2)
            , Font.weight 700
            , Shadow.text { offset = ( 1, 3 ), blur = 0, color = white }
            ]
        , style PurpleUnderline
            [ Font.underline
            , Color.decoration purple
            ]
        , style ActionList
            [ Color.text teal
            , Font.size (fontScale 2)
            , Font.weight 700
            ]
        , style AsSeenIn
            [ Color.background white
            , Font.size (fontScale 2)
            , Color.text (Color.rgb 78 72 72)
            ]
        , style About
            [ Color.background teal
            ]
        , style Challenge
            [ Font.size (fontScale 2)
            , Font.weight 700
            ]
        , style ChallengeInputLimit
            [ Color.border black
            , Border.all 2
            , Style.focus [ Color.border teal ]
            ]
        , style ChallengeInput
            [ Font.size (fontScale 1)
            , Color.border purple
            , Border.all 5
            , Style.focus [ Color.border teal ]
            ]
        , style SolutionCharBox
            [ Color.border black
            , Border.all 2
            , Font.typeface (fontStack Mono)
            ]
        , style SolutionCharBoxUsed
            [ Color.border teal
            , Border.all 2
            , Font.typeface (fontStack Mono)
            ]
        ]


fontScale : Int -> Float
fontScale =
    Scale.roundedModular 18 1.618


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
    "crockagile@gmail.com"


mailto : String
mailto =
    "mailto:" ++ email


view : Model -> Html.Html Msg
view model =
    Element.layout stylesheet <|
        column Page
            [ center, width fill, height fill ]
            [ header
            , actionCall
            , asSeenIn
            , about
            , challenge model
            ]


header : Element.Element Styles Cmd Msg
header =
    Element.header Header
        []
        (row NoStyle
            [ center, verticalCenter, width fill, height (px 85) ]
            [ Element.h1 Title [] (text "Jeff Wants Anchor")
            ]
        )


contestBanner : Element.Element Styles Cmd Msg
contestBanner =
    el ContestBanner
        [ width fill, center ]
        (link mailto <|
            (el NoStyle [ padding 6, center ] (text "Hire Jeff Crocker.  Find out how💰"))
        )


actionCall : Element.Element Styles Cmd Msg
actionCall =
    column ActionCall
        [ width fill, center, spacing 40, paddingBottom 60 ]
        [ contestBanner
        , Element.h2 ShadowedWhite [] (text "Writes code, right from your office.")
        , Element.h3 ActionList [] (text "Get products, cooperation, and passion, all for one salary.")
        , Element.paragraph ActionList
            []
            [ el NoStyle [ padding 6 ] (text "Already have engineers?")
            , link mailto <| (underline "Great, I work better in teams.")
            ]
        ]


asSeenIn : Element.Element Styles Cmd Msg
asSeenIn =
    column AsSeenIn
        [ spacing 20, center, paddingTop 25, paddingBottom 25 ]
        [ Element.h4 NoStyle
            []
            (text "As seen in...")
        , Element.wrappedRow
            NoStyle
            [ width fill, spacing 40 ]
            (List.map
                asSeenInLogo
                [ ( "polysync.png", "PolySync" )
                , ( "souperseconds.png", "Souper Seconds" )
                , ( "clearwater.png", "Clearwater Analytics" )
                , ( "camporkila.jpg", "Camp Orkila" )
                ]
            )
        ]


asSeenInLogo : ( String, String ) -> Element.Element Styles Cmd Msg
asSeenInLogo ( src, company ) =
    image NoStyle [ height (px 100) ] { src = src, caption = company ++ " Logo" }


about : Element.Element Styles Cmd Msg
about =
    row About
        [ width fill, center, padding 30 ]
        [ column NoStyle
            [ maxWidth (px 780), width fill ]
            [ Element.h2 ShadowedWhite [ padding 20 ] (text "About me")
            , textLayout ShadowedDark
                [ padding 20 ]
                [ paragraph NoStyle
                    []
                    [ text "As a regular podcast binger, I am on a mission to make your product a reality."
                    ]
                , paragraph NoStyle
                    [ paddingTop 20 ]
                    [ text "I am an enthusiastic engineer seeking to pin my curiosity and creativity against challenging problems."
                    , text " I know that any obstacle can be overcome with the ingenuity of an open-minded team."
                    ]
                , paragraph NoStyle
                    [ paddingTop 20 ]
                    [ text "My college curriculum taught me "
                    , el PurpleUnderline [] (text "C and unix systems programming")
                    , text ". But in my free time I learned "
                    , el PurpleUnderline [] (text "web development")
                    , text " and "
                    , el PurpleUnderline [] (text "Node.js")
                    , text ", which landed me a finance SaaS internship involving "
                    , el PurpleUnderline [] (text "REST")
                    , text " and "
                    , el PurpleUnderline [] (text "SQL")
                    , text "."
                    ]
                , paragraph NoStyle
                    [ paddingTop 20 ]
                    [ text "After graduation I leveraged my systems knowledge as a core engineer at an autonomous vehicle startup."
                    , text " While there I used "
                    , el PurpleUnderline [] (text "Unix/POSIX")
                    , text " to make efficient and reliable automotive sensor drivers and log files."
                    , text " I also used  "
                    , el PurpleUnderline [] (text "SQLite")
                    , text " to manage the vehicle's distributed configuration."
                    ]
                , paragraph NoStyle
                    [ paddingTop 20 ]
                    [ text "Outside of the office I continued to study new relevant or intriguing material."
                    , text " As a protection from system programming traps, "
                    , el PurpleUnderline [] (text "Rust")
                    , text " grabbed my attention and never let go."
                    , text " A helpful compiler and safety guarantees also pushed me towards learning "
                    , el PurpleUnderline [] (text "Haskell")
                    , text " and "
                    , el PurpleUnderline [] (text "Elm")
                    , text ", which I used to make this page!"
                    ]
                , paragraph NoStyle
                    [ paddingTop 20 ]
                    [ text "Want to work together? Email"
                    ]
                , paragraph ShadowedPurple
                    []
                    [ link mailto <| (text email) ]
                ]
            ]
        ]


challenge : Model -> Element.Element Styles Cmd Msg
challenge model =
    column Challenge
        [ width fill, maxWidth (px 780), center, padding 30, spacing 20 ]
        [ row NoStyle
            [ width fill, spread, spacing 10 ]
            [ Input.text ChallengeInputLimit
                []
                { onChange = NewChallengeLimit
                , value = toString initChallengeLimit
                , label = Input.labelAbove (text "Minimum Length")
                , options = [ Input.allowSpellcheck ]
                }
            , column NoStyle
                [ verticalSpread ]
                [ (text "Current Length")
                , (text <| toString <| String.length model.challengeText)
                ]
            ]
        , Input.multiline ChallengeInput
            [ height (px 300), padding 10 ]
            { onChange = NewChallengeText
            , value = initChallengeText
            , label =
                (Input.placeholder
                    { text = "Challenge Input Text"
                    , label = Input.hiddenLabel "Input:"
                    }
                )
            , options = []
            }
        , solution model
        ]


solution : Model -> Element.Element Styles Cmd Msg
solution model =
    let
        challengeTextDict =
            countCharacters model.challengeText

        charCounts =
            challengeTextDict
                |> Dict.toList
                |> List.sortBy Tuple.second

        extraLength =
            String.length model.challengeText - model.challengeLimit

        solution =
            Knapsack.solve_01 charCounts (always 1) (Tuple.second) extraLength
                |> Maybe.withDefault []

        usedChars =
            solution
                |> List.map Tuple.first
                |> Set.fromList

        solutionCost =
            List.map Tuple.second solution
                |> List.sum

        charCountsUsed =
            charCounts
                |> List.map (\( char, count ) -> ( char, count, Set.member char usedChars ))
    in
        Element.column NoStyle
            [ width fill, spacing 20 ]
            [ solutionChars charCountsUsed
            , column NoStyle
                [ verticalSpread ]
                [ (text "Removed Length")
                , (text (toString solutionCost))
                , (text "After Removal")
                , (paragraph ChallengeInput
                    [ height (px 300), padding 10 ]
                    [ text <|
                        String.filter
                            (\char ->
                                not <|
                                    Set.member char usedChars
                            )
                            model.challengeText
                    ]
                  )
                ]
            ]


solutionChars : List ( Char, Int, Bool ) -> Element.Element Styles Cmd Msg
solutionChars counts =
    Element.wrappedRow NoStyle
        [ spacing 5, spread ]
        (List.map
            solutionCharBox
            counts
        )


solutionCharBox : ( Char, Int, Bool ) -> Element.Element Styles Cmd Msg
solutionCharBox ( char, count, used ) =
    column
        (if used then
            SolutionCharBoxUsed
         else
            SolutionCharBox
        )
        [ center ]
        [ text (toString char)
        , text (toString count)
        ]


countCharacters : String -> Dict.Dict Char Int
countCharacters string =
    let
        incrementChar : Int -> Maybe Int -> Maybe Int
        incrementChar inc maybeCount =
            case maybeCount of
                Just n ->
                    Just (n + inc)

                Nothing ->
                    Just 1

        updateCounts : Char -> Dict.Dict Char Int -> Dict.Dict Char Int
        updateCounts char counts =
            Dict.update char (incrementChar 1) counts
    in
        string
            |> String.foldl updateCounts Dict.empty
