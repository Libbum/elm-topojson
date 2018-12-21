module TopoJson exposing (ArcIndex(..), Bbox, Geometry(..), Position(..), Properties, TopoJson(..), Topology, Transform, decode)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, dict, fail, field, float, int, list, maybe, string, succeed)
import Json.Encode as Json



-- List are good for changes, arrays are good for lookups. Makes sense that we make these arrays by default


type TopoJson
    = TopoJson Topology


type alias Topology =
    { objects : Dict String Geometry
    , arcs : List (List Position)
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
        (field "arcs" (list (list decodePosition)))
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
