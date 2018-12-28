module TopoJson exposing (ArcIndex(..), Bbox, Geometry(..), Position(..), Properties, TopoJson(..), Topology, Transform, arcTransform, bbBounds, bbGeometry, bbox, bboxPoint, decode, encode, transform)

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, array, dict, fail, field, float, int, list, maybe, string, succeed)
import Json.Encode as Json
import Maybe.Extra as Maybe



-- List are good for changes, arrays are good for lookups. Makes sense that we make some of these arrays by default.
-- Arcs for sure, but we'll hold off on the others and see what's best when we start to implement the operational portion of the library.


type TopoJson
    = TopoJson Topology


type alias Topology =
    { objects : Dict String Geometry
    , arcs : Array (Array Position)
    , transform : Maybe Transform
    , boundingbox : Maybe Bbox
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
    let
        trans =
            maybe <| field "transform" decodeTransform
    in
    Decode.map4 Topology
        (trans |> Decode.andThen (\t -> field "objects" (dict (decodeGeometry t))))
        (field "arcs" (array (array (decodePosition True))))
        trans
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


decodeGeometry : Maybe Transform -> Decoder Geometry
decodeGeometry trans =
    let
        quantised =
            case trans of
                Just _ ->
                    True

                Nothing ->
                    False
    in
    field "type" string |> Decode.andThen (geometryConvert quantised)


collectionConvert : Bool -> Decoder Geometry
collectionConvert quantised =
    field "type" string |> Decode.andThen (geometryConvert quantised)


geometryConvert : Bool -> String -> Decoder Geometry
geometryConvert quantised type_ =
    let
        properties =
            maybe <| field "properties" (dict string)
    in
    case type_ of
        "Point" ->
            Decode.map2 Point
                (field "coordinates" (decodePosition quantised))
                properties

        "MultiPoint" ->
            Decode.map2 MultiPoint
                (field "coordinates" (list (decodePosition quantised)))
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
                (field "geometries" (list (collectionConvert quantised)))

        _ ->
            fail <| "Unrecognized 'type': " ++ type_


decodePosition : Bool -> Decoder Position
decodePosition quantised =
    let
        listToPosition type_ ps =
            case ps of
                one :: two :: theRest ->
                    succeed <| type_ one two theRest

                _ ->
                    fail "Coordinate has too few values to make a position"
    in
    case quantised of
        True ->
            list int |> Decode.andThen (listToPosition Delta)

        False ->
            list float |> Decode.andThen (listToPosition Coordinate)


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
    let
        quantised =
            case topology.transform of
                Just _ ->
                    True

                Nothing ->
                    False
    in
    Json.object
        [ ( "type", Json.string "Topology" )
        , ( "objects", Json.dict identity (encodeGeometry quantised) topology.objects )
        , ( "arcs", Json.list (\a -> Json.list (encodePosition True) (Array.toList a)) (Array.toList topology.arcs) )
        , ( "transform", encodeTransform topology.transform )
        , ( "bbox", encodeBbox topology.boundingbox )
        ]


encodeGeometry : Bool -> Geometry -> Json.Value
encodeGeometry quantised geometry =
    case geometry of
        Point position properties ->
            Json.object
                [ ( "type", Json.string "Point" )
                , ( "coordinates", encodePosition quantised position )
                , ( "properties", encodeProperties properties )
                ]

        MultiPoint positions properties ->
            Json.object
                [ ( "type", Json.string "MultiPoint" )
                , ( "coordinates", Json.list (encodePosition quantised) positions )
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
                , ( "geometries", Json.list (encodeGeometry quantised) geometries )
                ]


encodeBbox : Maybe Bbox -> Json.Value
encodeBbox box =
    case box of
        Just data ->
            Json.list Json.float data

        Nothing ->
            Json.null


encodeTransform : Maybe Transform -> Json.Value
encodeTransform trans =
    case trans of
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


encodePosition : Bool -> Position -> Json.Value
encodePosition quantised position =
    case quantised of
        True ->
            case position of
                Delta one two theRest ->
                    Json.list Json.int (one :: two :: theRest)

                --TODO: This is not handled correctly
                Coordinate _ _ _ ->
                    Json.null

        False ->
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



--- CLIENT
--- TRANSFORMS


transform : Maybe Transform -> Position -> Position
transform trans current =
    case ( trans, current ) of
        ( Just t, Delta one two theRest ) ->
            let
                kx =
                    Tuple.first t.scale

                ky =
                    Tuple.second t.scale

                dx =
                    Tuple.first t.translate

                dy =
                    Tuple.second t.translate
            in
            Coordinate (toFloat one * kx + dx) (toFloat two * ky + dy) (List.map toFloat theRest)

        ( Nothing, Delta one two theRest ) ->
            Coordinate (toFloat one) (toFloat two) (List.map toFloat theRest)

        _ ->
            current


bbox : Topology -> Maybe { xmin : Float, xmax : Float, ymin : Float, ymax : Float }
bbox topology =
    let
        objectBounds =
            topology.objects
                |> Dict.values
                |> List.map (bbGeometry topology.transform)
                |> List.concat
                |> Maybe.values
                |> List.unzip
                |> bbBounds

        arcs =
            topology.arcs
                |> Array.map Array.toList
                |> Array.toList

        _ =
            Debug.log "arcs" arcs
    in
    objectBounds


arcTransform : Maybe Transform -> Maybe { xmin : Float, xmax : Float, ymin : Float, ymax : Float } -> List Position -> Float
arcTransform trans bounds arc =
    case ( trans, bounds ) of
        ( Just t, Just b ) ->
            let
                kx =
                    Tuple.first t.scale

                ky =
                    Tuple.second t.scale

                dx =
                    Tuple.first t.translate

                dy =
                    Tuple.second t.translate
            in
            List.foldl
                (\pos x0 ->
                    case pos of
                        Delta one _ _ ->
                            let
                                new =
                                    (toFloat one + x0) * kx + dx

                                _ =
                                    Debug.log "new" new

                                _ =
                                    Debug.log "x0" x0
                            in
                            if new < x0 then
                                new

                            else
                                x0

                        _ ->
                            x0
                )
                b.xmin
                arc

        _ ->
            0.0


bbGeometry : Maybe Transform -> Geometry -> List (Maybe ( Float, Float ))
bbGeometry trans object =
    case object of
        Point position _ ->
            [ bboxPoint <| transform trans position ]

        MultiPoint positions _ ->
            List.map (\position -> bboxPoint <| transform trans position) positions

        GeometryCollection geometries ->
            List.map (bbGeometry trans) geometries
                |> List.concat

        _ ->
            []


bboxPoint : Position -> Maybe ( Float, Float )
bboxPoint position =
    case position of
        Coordinate one two _ ->
            Just ( one, two )

        _ ->
            Nothing


bbBounds : ( List Float, List Float ) -> Maybe { xmin : Float, xmax : Float, ymin : Float, ymax : Float }
bbBounds ( xvals, yvals ) =
    let
        bounds =
            [ List.minimum xvals, List.maximum xvals, List.minimum yvals, List.maximum yvals ]
                |> Maybe.values
    in
    case bounds of
        [ xmin, xmax, ymin, ymax ] ->
            Just { xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax }

        _ ->
            Nothing
