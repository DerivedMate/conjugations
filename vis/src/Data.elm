module Data exposing (Data, decodeData)

import Json.Decode as D exposing (Decoder)
import Http


-- import Json.Encode as E -- Skip encoding, as the data is going to be read-only


type alias L3 a =
    List (L2 a)


l3Decode : D.Decoder a -> D.Decoder (L3 a)
l3Decode aDecoder =
    D.list (D.list (D.list aDecoder))

type alias L2 a =
    List (L a)


l2Decode : D.Decoder a -> D.Decoder (L2 a)
l2Decode aDecoder =
    D.list (D.list aDecoder)


type alias L a =
    List a


lDecode : D.Decoder a -> D.Decoder (L a)
lDecode aDecoder =
    D.list aDecoder


type Idd a
    = Idd Int a


iddDecode : D.Decoder a -> D.Decoder (Idd a)
iddDecode aDecoder =
    D.map2 Idd
        (D.field "id" D.int)
        (D.field "data" aDecoder)


type Group a g
    = Group (List a) g


groupDecode : D.Decoder g -> D.Decoder (Group String g)
groupDecode gDecoder =
    D.map2 Group
        (D.field "members" (D.list D.string))
        (D.field "group" gDecoder)


type Category
    = Category (List String) String


categoryDecode : Decoder Category
categoryDecode =
    D.map2 Category
        (D.field "transformations" (D.list D.string))
        (D.field "suffix" D.string)


type alias Data =
    { l2 : List (Idd (Group String (L3 Category)))
    , l1 : List (Idd (Group String (L2 Category)))
    , l0 : List (Idd (Group String (L Category)))
    }


decodeData : D.Decoder Data
decodeData =
    D.map3 Data
        (D.field "l2" (D.list (iddDecode (groupDecode (l3Decode categoryDecode)))))
        (D.field "l1" (D.list (iddDecode (groupDecode (l2Decode categoryDecode)))))
        (D.field "l1" (D.list (iddDecode (groupDecode (lDecode categoryDecode)))))