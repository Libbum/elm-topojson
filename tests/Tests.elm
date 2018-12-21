module Tests exposing (encodeAndDecode)

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Json.Decode exposing (decodeString)
import Json.Encode
import Test exposing (..)
import TopoJson exposing (ArcIndex(..), Bbox, Geometry(..), Position(..), Properties, TopoJson(..), Topology, Transform)


emptyObject : Json.Encode.Value
emptyObject =
    Json.Encode.object []


encodeAndDecode : Test
encodeAndDecode =
    fuzzWith { runs = 30 } fuzzTopoJson "encoding and decoding does not change the TopoJson" <|
        \topojson ->
            TopoJson.encode topojson |> Json.Decode.decodeValue TopoJson.decode |> Expect.equal (Ok topojson)


fuzzTopoJson : Fuzzer TopoJson
fuzzTopoJson =
    Fuzz.map TopoJson fuzzTopology


fuzzTopology : Fuzzer Topology
fuzzTopology =
    Fuzz.map4 Topology fuzzGeometryDict (Fuzz.array (Fuzz.array fuzzPosition)) fuzzTransform fuzzBbox


fuzzGeometryDict : Fuzzer (Dict String Geometry)
fuzzGeometryDict =
    Fuzz.map Dict.fromList fuzzDictPair


fuzzDictPair : Fuzzer (List ( String, Geometry ))
fuzzDictPair =
    ( Fuzz.string, fuzzGeometry )
        |> Fuzz.tuple
        |> Fuzz.list


fuzzGeometry : Fuzzer Geometry
fuzzGeometry =
    let
        helper depth =
            Fuzz.frequency
                [ ( 3, Fuzz.map (\p -> Point p Nothing) fuzzPosition )
                , ( 3, Fuzz.map (\p -> MultiPoint p Nothing) (Fuzz.list fuzzPosition) )
                , ( 3, Fuzz.map (\p -> LineString p Nothing) fuzzArcIndex )
                , ( 3, Fuzz.map (\p -> MultiLineString p Nothing) (Fuzz.list fuzzArcIndex) )
                , ( 3, Fuzz.map (\p -> Polygon p Nothing) (Fuzz.list fuzzArcIndex) )
                , ( 3, Fuzz.map (\xs -> MultiPolygon [ xs ] Nothing) (Fuzz.list fuzzArcIndex) )
                , if depth > 2 then
                    ( 0, Fuzz.constant (Point (Delta 2 3 [ 1, 3 ]) Nothing) )

                  else
                    ( 1 / depth, Fuzz.map GeometryCollection (Fuzz.list (helper (depth + 1))) )
                ]
    in
    helper 2


fuzzTransform : Fuzzer (Maybe Transform)
fuzzTransform =
    Fuzz.map4 (\a b c d -> { scale = ( a, b ), translate = ( c, d ) }) Fuzz.float Fuzz.float Fuzz.float Fuzz.float
        |> Fuzz.maybe


fuzzPosition : Fuzzer Position
fuzzPosition =
    Fuzz.frequency
        [ ( 3, Fuzz.map2 (\a b -> Coordinate a b []) Fuzz.float Fuzz.float )
        , ( 3, Fuzz.map2 (\a b -> Delta a b []) Fuzz.int Fuzz.int )
        ]


fuzzArcIndex : Fuzzer ArcIndex
fuzzArcIndex =
    Fuzz.map2 (\a b -> ArcIndex a b) Fuzz.int (Fuzz.list Fuzz.int)


fuzzBbox : Fuzzer (Maybe Bbox)
fuzzBbox =
    Fuzz.list Fuzz.float |> Fuzz.maybe
