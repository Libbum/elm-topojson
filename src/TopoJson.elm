module TopoJson exposing (ArcIndex(..), Bbox, Geometry(..), Position(..), Properties, TopoJson(..), Topology, Transform, decode, encode)

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, array, dict, fail, field, float, int, list, maybe, string, succeed)
import Json.Encode as Json



-- List are good for changes, arrays are good for lookups. Makes sense that we make some of these arrays by default.
-- Arcs for sure, but we'll hold off on the others and see what's best when we start to implement the operational portion of the library.


type TopoJson
    = TopoJson Topology


type alias Topology =
    { objects : Dict String Geometry
    , arcs : Array (Array Position)
    , transform : Maybe Transform
    , bbox : Maybe Bbox
    }


type Geometry
    = Point Position (Maybe Properties)
    | MultiPoint (List Position) (Maybe Properties)
    | LineString ArcIndex (Maybe Properties)
    | MultiLineString (List ArcIndex) (Maybe Properties)
    | Polygon (List ArcIndex) (Maybe Properties)
    | MultiPolygon (List (List ArcIndex)) (Maybe Properties)
    | GeometryCollection (List Geometry)


type alias Properties =
    Dict String String


type alias Bbox =
    List Float


type alias Transform =
    { scale : ( Float, Float )
    , translate : ( Float, Float )
    }


type Position
    = Delta Int Int (List Int)
    | Coordinate Float Float (List Float)


type ArcIndex
    = ArcIndex Int (List Int)



--- DECODER


decode : Decoder TopoJson
decode =
    let
        converter type_ =
            case type_ of
                "Topology" ->
                    Decode.map TopoJson decodeTopology

                _ ->
                    fail "Invalid TopoJSON type"
    in
    field "type" string
        |> Decode.andThen converter


decodeTopology : Decoder Topology
decodeTopology =
    Decode.map4 Topology
        (field "objects" (dict decodeGeometry))
        (field "arcs" (array (array decodePosition)))
        (maybe <| field "transform" decodeTransform)
        (maybe <| field "bbox" decodeBbox)


{-| Type is a simple list, but checks should be done to verify its size as 2\*n (where n is the # dimensions in the contained geometry).
-}
decodeBbox : Decoder Bbox
decodeBbox =
    list float


decodeTransform : Decoder Transform
decodeTransform =
    Decode.map2 Transform
        (field "scale" decodeFloatTuple)
        (field "translate" decodeFloatTuple)


decodeFloatTuple : Decoder ( Float, Float )
decodeFloatTuple =
    let
        errorString adj =
            "Transform has too " ++ adj ++ " values in matrix"

        listToTuple ps =
            case ps of
                [] ->
                    fail (errorString "few")

                [ _ ] ->
                    fail (errorString "few")

                [ p1, p2 ] ->
                    succeed ( p1, p2 )

                _ ->
                    fail (errorString "many")
    in
    list float |> Decode.andThen listToTuple


decodeGeometry : Decoder Geometry
decodeGeometry =
    let
        properties =
            maybe <| field "properties" (dict string)

        helper type_ =
            case type_ of
                "Point" ->
                    Decode.map2 Point
                        (field "coordinates" decodePosition)
                        properties

                "MultiPoint" ->
                    Decode.map2 MultiPoint
                        (field "coordinates" (list decodePosition))
                        properties

                "LineString" ->
                    Decode.map2 LineString
                        (field "arcs" decodeArcIndex)
                        properties

                "MultiLineString" ->
                    Decode.map2 MultiLineString
                        (field "arcs" (list decodeArcIndex))
                        properties

                "Polygon" ->
                    Decode.map2 Polygon
                        (field "arcs" (list decodeArcIndex))
                        properties

                "MultiPolygon" ->
                    Decode.map2 MultiPolygon
                        (field "arcs" (list (list decodeArcIndex)))
                        properties

                "GeometryCollection" ->
                    Decode.map GeometryCollection
                        (field "geometries" (list decodeGeometry))

                _ ->
                    fail <| "Unrecognized 'type': " ++ type_
    in
    field "type" string
        |> Decode.andThen helper


decodePosition : Decoder Position
decodePosition =
    let
        listToPosition type_ ps =
            case ps of
                one :: two :: theRest ->
                    succeed <| type_ one two theRest

                _ ->
                    fail "Coordinate has too few values to make a position"
    in
    Decode.oneOf
        [ list int |> Decode.andThen (listToPosition Delta)
        , list float |> Decode.andThen (listToPosition Coordinate)
        ]


decodeArcIndex : Decoder ArcIndex
decodeArcIndex =
    let
        helper ps =
            case ps of
                one :: theRest ->
                    succeed <| ArcIndex one theRest

                _ ->
                    fail "ArcIndex must have at least one value"
    in
    list int |> Decode.andThen helper



--- ENCODER


encode : TopoJson -> Json.Value
encode topojson =
    case topojson of
        TopoJson topology ->
            encodeTopology topology


encodeTopology : Topology -> Json.Value
encodeTopology topology =
    Json.object
        [ ( "type", Json.string "Topology" )
        , ( "objects", Json.dict identity encodeGeometry topology.objects )
        , ( "arcs", Json.list (\a -> Json.list encodePosition (Array.toList a)) (Array.toList topology.arcs) )
        , ( "transform", encodeTransform topology.transform )
        , ( "bbox", encodeBbox topology.bbox )
        ]


encodeGeometry : Geometry -> Json.Value
encodeGeometry geometry =
    case geometry of
        Point position properties ->
            Json.object
                [ ( "type", Json.string "Point" )
                , ( "coordinates", encodePosition position )
                , ( "properties", encodeProperties properties )
                ]

        MultiPoint positions properties ->
            Json.object
                [ ( "type", Json.string "MultiPoint" )
                , ( "coordinates", Json.list encodePosition positions )
                , ( "properties", encodeProperties properties )
                ]

        LineString arcindex properties ->
            Json.object
                [ ( "type", Json.string "LineString" )
                , ( "arcs", encodeArcIndex arcindex )
                , ( "properties", encodeProperties properties )
                ]

        MultiLineString arcindecies properties ->
            Json.object
                [ ( "type", Json.string "MultiLineString" )
                , ( "arcs", Json.list encodeArcIndex arcindecies )
                , ( "properties", encodeProperties properties )
                ]

        Polygon arcindecies properties ->
            Json.object
                [ ( "type", Json.string "Polygon" )
                , ( "arcs", Json.list encodeArcIndex arcindecies )
                , ( "properties", encodeProperties properties )
                ]

        MultiPolygon arcindecies properties ->
            Json.object
                [ ( "type", Json.string "MultiPolygon" )
                , ( "arcs", Json.list (Json.list encodeArcIndex) arcindecies )
                , ( "properties", encodeProperties properties )
                ]

        GeometryCollection geometries ->
            Json.object
                [ ( "type", Json.string "GeometryCollection" )
                , ( "geometries", Json.list encodeGeometry geometries )
                ]


encodeBbox : Maybe Bbox -> Json.Value
encodeBbox bbox =
    case bbox of
        Just data ->
            Json.list Json.float data

        Nothing ->
            Json.null


encodeTransform : Maybe Transform -> Json.Value
encodeTransform transform =
    case transform of
        Just data ->
            let
                s =
                    data.scale

                t =
                    data.translate

                scale =
                    [ Tuple.first s, Tuple.second s ]

                translate =
                    [ Tuple.first t, Tuple.second t ]
            in
            Json.object
                [ ( "scale", Json.list Json.float scale )
                , ( "translate", Json.list Json.float translate )
                ]

        Nothing ->
            Json.null


encodeProperties : Maybe Properties -> Json.Value
encodeProperties properties =
    case properties of
        Just data ->
            Json.dict identity Json.string data

        Nothing ->
            Json.null


encodePosition : Position -> Json.Value
encodePosition position =
    case position of
        Delta one two theRest ->
            Json.list Json.int (one :: two :: theRest)

        Coordinate one two theRest ->
            Json.list Json.float (one :: two :: theRest)


encodeArcIndex : ArcIndex -> Json.Value
encodeArcIndex arcindex =
    let
        indicies =
            case arcindex of
                ArcIndex one theRest ->
                    one :: theRest
    in
    Json.list Json.int indicies
