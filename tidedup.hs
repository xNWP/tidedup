-- tidedup
--
-- Author: Brett "xNWP" Anthony <nwpbusiness@gmail.com>
-- This Haskell program takes from the std input a newline separated list of marked tiles
-- exported from the RuneLite TileIndicators plugin, it then removes duplicate tiles (tiles
-- residing on the same x y z) and prints out the unique tiles in a way that it may be copy-pasted
-- and imported back into RuneLite.
--
-- Usage (Windows):
--   cat .\tilelist.txt | .\tidedup.exe
--   cat .\tilelist.txt | .\tidedup.exe > uniquetiles.txt

import qualified Data.Map as Map
import Data.Function (on)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.List (intercalate)
import Data.Char (toLower)
import System.Environment (getArgs)

version = [1, 0] -- version 1.0

main = do
    contents <- getContents
    args <- getArgs

    if (not . null) args then
        putStr helpText
    else do
        let parse_list = map (fromJust . parse) :: [Map.Map String String] -> [Tile]
        let unique_tiles = dedup $ concatMap (parse_list . fst . parseArrayOfStructs) $ lines contents
        putStrLn $ "[" ++ intercalate "," (map tileToRuneliteTile unique_tiles) ++ "]"

helpText = "tidedup v" ++ (show . head) version ++ "." ++ show (version !! 1) ++ "\n\
        \\n\
        \Author: Brett \"xNWP\" Anthony <nwpbusiness@gmail.com>\n\
        \This Haskell program takes from the std input a newline separated list of marked tiles\n\
        \exported from the RuneLite TileIndicators plugin, it then removes duplicate tiles (tiles\n\
        \residing on the same x y z) and prints out the unique tiles in a way that it may be copy-pasted\n\
        \and imported back into RuneLite.\n\
        \\n\
        \Usage (Windows):\n\
        \  cat .\\tilelist.txt | .\\tidedup.exe\n\
        \  cat .\\tilelist.txt | .\\tidedup.exe > uniquetiles.txt\n"

data Tile = Tile {
    tileBase :: TileBase,
    label :: Maybe String
} deriving (Show, Ord)

data TileBase = TileBase {
    regionId :: Int,
    regionX :: Int,
    regionY :: Int,
    z :: Int,
    color :: String
} deriving (Show, Ord)

instance Eq Tile where
    (==) :: Tile -> Tile -> Bool
    (==) = fieldEqual tileBase

instance Eq TileBase where
    t1 == t2 =
        fieldEqual regionId t1 t2 &&
        fieldEqual regionX t1 t2 &&
        fieldEqual regionY t1 t2 &&
        fieldEqual z t1 t2

fieldEqual :: Eq b => (a -> b) -> a -> a -> Bool
fieldEqual field = (==) `on` field

class Parseable a where
    parse :: Map.Map String String -> Maybe a

instance Parseable Tile where
    parse m = tileParseSub field_values where
        fields = ["regionId", "regionX", "regionY", "z", "color", "label"]
        field_values = map (`Map.lookup` m) fields

tileParseSub :: [Maybe String] -> Maybe Tile
tileParseSub [Just regionId, Just regionX, Just regionY, Just z, Just color, maybe_label] = Just Tile {
    tileBase = TileBase {
        regionId = read regionId,
        regionX = read regionX,
        regionY = read regionY,
        z = read z,
        color = color
    },
    label = maybe_label
}

tileParseSub _ = error "Malformed tile data."

parseField :: String -> (String, String, String)
parseField = parseFieldSub0

parseFieldSub0 ('"' : rest) = (name, value, rest3) where
    (name, rest2) = breakEq '"' rest
    (value, rest3) = parseFieldSub1 rest2

parseFieldSub1 ('"' : ':' : rest) = break (`elem` ['}', ',']) rest

breakEq :: Eq a => a -> [a] -> ([a], [a])
breakEq ch = break (==ch)

parseStruct :: String -> (Map.Map String String, String)
parseStruct = parseStructSub0

parseStructSub0 ('{' : '}' : rest) = (Map.empty, rest)
parseStructSub0 ('{' : rest) = parseStructSub1 Map.empty rest

parseStructSub1 accum input = if head rest == ',' then
        parseStructSub1 accum2 (tail rest)
    else
        (accum2, tail rest)
    where
        (name, value, rest) = parseField input
        accum2 = Map.insert name value accum

parseArrayOfStructs :: String -> ([Map.Map String String], String)
parseArrayOfStructs = parseArrayOfStructsSub0

parseArrayOfStructsSub0 ('[' : ']' : rest) = ([], rest)
parseArrayOfStructsSub0 ('[' : rest) = parseArrayOfStructsSub1 [] rest

parseArrayOfStructsSub1 accum input = if head rest == ',' then
        parseArrayOfStructsSub1 accum2 (tail rest)
    else
        (accum2, tail rest)
    where
        (struct, rest) = parseStruct input
        accum2 = struct : accum

dedup :: Ord a => [a] -> [a]
dedup = Set.toList . Set.fromList

tileToRuneliteTile :: Tile -> String
tileToRuneliteTile = writeStruct . tileToStruct

data StructField = StructField {
    name :: String,
    value :: StructValueType
}
data StructValueType = StructInt Int | StructString String

makeStructFieldInt name value = StructField {
    name = name,
    value = StructInt value
}

makeStructFieldString name value = StructField {
    name = name,
    value = StructString value
}

tileBaseToStruct :: TileBase -> [StructField]
tileBaseToStruct TileBase {
    regionId,
    regionX,
    regionY,
    z,
    color
} = [
        makeStructFieldInt "regionId" regionId,
        makeStructFieldInt "regionX" regionX,
        makeStructFieldInt "regionY" regionY,
        makeStructFieldInt "z" z,
        makeStructFieldString "color" color
    ]

tileToStruct :: Tile -> [StructField]
tileToStruct Tile {
    tileBase,
    label = Nothing
} = tileBaseToStruct tileBase

tileToStruct Tile {
    tileBase,
    label = Just lbl
} = tileBaseToStruct tileBase ++ [makeStructFieldString "label" lbl]

writeStruct :: [StructField] -> String
writeStruct fields = ('{' : intercalate "," (map writeStructField fields)) ++ ['}']

writeStructField :: StructField -> String
writeStructField StructField {
    name,
    value
} = "\"" ++ name ++ "\":" ++ writeStructFieldValue value

writeStructFieldValue :: StructValueType -> String
writeStructFieldValue (StructInt i) = show i
writeStructFieldValue (StructString s) = s