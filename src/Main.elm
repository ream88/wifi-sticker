module Main exposing (main)

import Browser
import Heroicons.Solid
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as JD
import QRCode
import Svg exposing (svg)
import Svg.Attributes


type WiFiEncryption
    = None
    | WEP
    | WPA


type alias WiFi =
    { ssid : String
    , password : String
    , encryption : WiFiEncryption
    , hidden : Bool
    }


type alias Model =
    { wifi : WiFi
    , passwordVisible : Bool
    , advancedOptionsVisible : Bool
    }


type alias Flags =
    ()


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "WiFi Sticker", body = [ view model ] }
        , update = update
        , subscriptions = always Sub.none
        }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { wifi =
            { ssid = ""
            , password = ""
            , encryption = WPA
            , hidden = False
            }
      , passwordVisible = False
      , advancedOptionsVisible = False
      }
    , Cmd.none
    )


type Msg
    = SetWiFiSSID String
    | SetWiFiPassword String
    | ToggleWiFiVisibility
    | SetWiFiEncryption WiFiEncryption
    | TogglePassword
    | ToggleAdvancedOptions


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetWiFiSSID ssid ->
            let
                newWifi =
                    setSSID ssid model.wifi
            in
            ( { model | wifi = newWifi }, Cmd.none )

        SetWiFiPassword ssid ->
            let
                newWifi =
                    setPassword ssid model.wifi
            in
            ( { model | wifi = newWifi }, Cmd.none )

        ToggleWiFiVisibility ->
            let
                newWifi =
                    toggleVisibility model.wifi
            in
            ( { model | wifi = newWifi }, Cmd.none )

        SetWiFiEncryption encryption ->
            let
                newWifi =
                    setEncryption encryption model.wifi
            in
            ( { model | wifi = newWifi }, Cmd.none )

        TogglePassword ->
            ( { model | passwordVisible = not model.passwordVisible }, Cmd.none )

        ToggleAdvancedOptions ->
            ( { model | advancedOptionsVisible = not model.advancedOptionsVisible }, Cmd.none )


setSSID : String -> WiFi -> WiFi
setSSID ssid wifi =
    { wifi | ssid = ssid }


setPassword : String -> WiFi -> WiFi
setPassword password wifi =
    { wifi | password = password }


toggleVisibility : WiFi -> WiFi
toggleVisibility wifi =
    { wifi | hidden = not wifi.hidden }


setEncryption : WiFiEncryption -> WiFi -> WiFi
setEncryption encryption wifi =
    { wifi | encryption = encryption }


hasNoEncryption : WiFi -> Bool
hasNoEncryption wifi =
    case wifi.encryption of
        None ->
            True

        _ ->
            False


hasWEPEncryption : WiFi -> Bool
hasWEPEncryption wifi =
    case wifi.encryption of
        WEP ->
            True

        _ ->
            False


hasWPAEncryption : WiFi -> Bool
hasWPAEncryption wifi =
    case wifi.encryption of
        WPA ->
            True

        _ ->
            False


view : Model -> Html Msg
view model =
    div [ class "flex items-center justify-center min-h-screen bg-gray-50" ]
        [ main_ [ class "px-8 py-10 bg-white rounded-md shadow" ]
            [ h1 [ class "flex items-center justify-center gap-3 mb-8 text-4xl" ]
                [ Heroicons.Solid.wifi [ Svg.Attributes.class "w-10 h-10" ]
                , span [] [ text "Wi-Fi Sticker" ]
                ]
            , div [ class "flex items-stretch gap-8" ]
                [ viewForm model
                , aside [ class "flex flex-col items-end justify-between flex-1" ]
                    [ viewQRCode model
                    , div [ class "inline-flex gap-2 mt-10" ]
                        [ button [ type_ "submit", class "inline-flex items-center justify-center gap-2 px-4 py-2 font-medium text-white bg-indigo-600 border border-transparent rounded-md shadow-sm hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500" ]
                            [ Heroicons.Solid.download [ Svg.Attributes.class "w-5 h-5" ]
                            , text "Download"
                            ]
                        , button [ type_ "submit", class "inline-flex items-center justify-center gap-2 px-4 py-2 font-medium text-white bg-indigo-600 border border-transparent rounded-md shadow-sm hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500" ]
                            [ Heroicons.Solid.printer [ Svg.Attributes.class "w-5 h-5" ]
                            , text "Print"
                            ]
                        ]
                    ]
                ]
            ]
        ]


viewForm : Model -> Html Msg
viewForm model =
    Html.form [ class "w-80" ]
        (viewBasicOptions model
            ++ (if model.advancedOptionsVisible then
                    viewAdvancedOptions model

                else
                    []
               )
        )


viewBasicOptions : Model -> List (Html Msg)
viewBasicOptions model =
    [ p [ class "text-sm text-gray-500" ]
        [ text "The information entered below will never leave this website." ]
    , div [ class "mt-5" ]
        [ label [ for "ssid", class "block text-sm font-medium text-gray-700" ]
            [ text "Wi-Fi network name (or SSID)" ]
        , input
            [ type_ "text"
            , id "ssid"
            , autocomplete False
            , class "block w-full mt-1 border-gray-300 rounded-md shadow-sm focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm"
            , value model.wifi.ssid
            , placeholder "skynet"
            , onInput SetWiFiSSID
            ]
            []
        ]
    , div [ class "mt-5" ]
        [ label [ for "password", class "block text-sm font-medium text-gray-700" ]
            [ text "Password" ]
        , div [ class "relative mt-1 rounded-md shadow-sm" ]
            [ input
                [ type_
                    (if model.passwordVisible then
                        "text"

                     else
                        "password"
                    )
                , id "password"
                , autocomplete False
                , class "block w-full pr-10 border-gray-300 rounded-md focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm"
                , value model.wifi.password
                , placeholder "something-secure"
                , onInput SetWiFiPassword
                ]
                []
            , button [ type_ "button", class "absolute inset-y-0 right-0 flex items-center pr-3", onClick TogglePassword ]
                [ if model.passwordVisible then
                    Heroicons.Solid.eyeOff [ Svg.Attributes.class "w-5 h-5 text-gray-500" ]

                  else
                    Heroicons.Solid.eye [ Svg.Attributes.class "w-5 h-5 text-gray-500" ]
                ]
            ]
        ]
    , if model.advancedOptionsVisible then
        text ""

      else
        div [ class "mt-5" ]
            [ button [ type_ "button", class "text-sm text-indigo-500", onClick ToggleAdvancedOptions ]
                [ text "Show advanced options" ]
            ]
    ]


viewAdvancedOptions : Model -> List (Html Msg)
viewAdvancedOptions model =
    [ div [ class "mt-5" ]
        [ p [ class "text-sm text-gray-500" ]
            [ text "You can probably leave these settings on their default values." ]
        ]
    , div [ class "mt-5" ]
        [ div []
            [ label [ for "encryption", class "block text-sm font-medium text-gray-700" ]
                [ text "Encryption" ]
            , select
                [ id "encryption"
                , class "block w-full py-2 pl-3 pr-10 mt-1 text-base border-gray-300 rounded-md focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm"
                , onEncryptionInput SetWiFiEncryption
                ]
                [ option [ value "None", selected (hasNoEncryption model.wifi) ] [ text "None" ]
                , option [ value "WEP", selected (hasWEPEncryption model.wifi) ] [ text "WEP" ]
                , option [ value "WPA", selected (hasWPAEncryption model.wifi) ] [ text "WPA" ]
                ]
            ]
        ]
    , div [ class "mt-5" ]
        [ div []
            [ div [ class "relative flex items-start" ]
                [ div [ class "flex items-center h-5" ]
                    [ input
                        [ type_ "checkbox"
                        , id "hidden-network"
                        , class "w-4 h-4 text-indigo-600 border-gray-300 rounded focus:ring-indigo-500"
                        , checked model.wifi.hidden
                        , onCheck (always ToggleWiFiVisibility)
                        ]
                        []
                    ]
                , div [ class "ml-2 text-sm" ]
                    [ label [ for "hidden-network", class "font-medium text-gray-700" ]
                        [ text "Hidden network" ]
                    ]
                ]
            ]
        ]
    ]


onEncryptionInput : (WiFiEncryption -> Msg) -> Attribute Msg
onEncryptionInput tagger =
    targetValue
        |> JD.andThen parseEncryption
        |> JD.map tagger
        |> JD.map alwaysStop
        |> stopPropagationOn "input"


alwaysStop : msg -> ( msg, Bool )
alwaysStop msg =
    ( msg, True )


parseEncryption : String -> JD.Decoder WiFiEncryption
parseEncryption encryption =
    case encryption of
        "None" ->
            JD.succeed None

        "WEP" ->
            JD.succeed WEP

        "WPA" ->
            JD.succeed WPA

        other ->
            JD.fail ("\"" ++ other ++ " is not a valid Wi-Fi encryption")


viewQRCode : Model -> Html Msg
viewQRCode model =
    model.wifi
        |> generateWifiString
        |> QRCode.fromString
        |> Result.map
            (QRCode.toSvgWithoutQuietZone
                [ Svg.Attributes.width "300px"
                , Svg.Attributes.height "300px"
                , Svg.Attributes.class "p-8 border rounded-md"
                ]
            )
        |> Result.withDefault (Html.text "")


generateWifiString : WiFi -> String
generateWifiString wifi =
    "WIFI:"
        ++ String.join ";"
            [ case wifi.encryption of
                None ->
                    "T:nopass"

                WEP ->
                    "T:WEP"

                WPA ->
                    "T:WPA2"
            , "S:" ++ wifi.ssid
            , "P:" ++ wifi.password
            , if wifi.hidden then
                "H:true"

              else
                "H:false"
            ]
