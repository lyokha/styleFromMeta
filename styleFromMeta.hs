{-# LANGUAGE ViewPatterns, PatternGuards, PatternSynonyms #-}

import           Text.Pandoc.JSON
import           Text.Pandoc.Walk (walk)
import           Text.Pandoc.Options (def)
import           Control.Exception (displayException)
import qualified Data.Map as M
import           Data.String.Utils (replace)

#if MIN_VERSION_pandoc(2,0,0)
import           Text.Pandoc.Writers (Writer (..), getWriter)
import           Text.Pandoc.Class (runPure)
import qualified Data.ByteString.Lazy.Char8 as C8L
import qualified Data.Text as T

#define MBPLAIN Plain
#else
import           Text.Pandoc (Writer (..), getWriter)

#define MBPLAIN Para
#endif

pattern Style :: String -> Inline
pattern Style x <- Math InlineMath x

pattern Alt :: [Inline] -> [Inline]
pattern Alt x <- (dropWhile (== Space) -> x)

type MMap = M.Map String MetaValue
type PureInlineParams = ([Inline], Target)          -- style:(alt, target)
type InlineParams = (Inline, [Inline], Target)      -- (style:alt, target)
type InlineCons = [Inline] -> Target -> Inline      -- Image or Link

styleFromMeta :: Maybe Format -> Pandoc -> IO Pandoc
styleFromMeta (Just fm) (Pandoc m bs) =
    return $ Pandoc m $ walk (substStyle fm $ unMeta m) bs
styleFromMeta _ p = return p

substStyle :: Format -> MMap -> Block -> Block
substStyle fm@(Format fmt) m b@(Para [Image attr (Style style : Alt alt) tgt])
    | Just (MetaMap mm) <- M.lookup style m =
        let params = (alt, tgt)
            substStyle' (Just (MetaBlocks [RawBlock f s])) =
                RawBlock f $ substParams fm params s
            substStyle' (Just (MetaBlocks [b])) = walk substParams' b
                where substParams' (RawInline f s) =
                            RawInline f $ substParams fm params s
                      substParams' i = i
            substStyle' Nothing = Para [Image attr alt tgt]
            substStyle' _ = b
        in substStyle' $ M.lookup fmt mm
    | otherwise = b
substStyle fm@(Format fmt) m (Para cnt)
    | Just (MetaMap mm) <- M.lookup "para_style" m
    , Just (MetaBlocks [MBPLAIN [Span attr _]]) <- M.lookup fmt mm =
        walk (substInlineStyle fm m) $ Plain [Span attr cnt]
substStyle fm m b = walk (substInlineStyle fm m) b

substInlineStyle :: Format -> MMap -> Inline -> Inline
substInlineStyle fm@(Format fmt) m
                 i@(toInlineParams -> Just ((Style style, alt, tgt), cons))
    | Just (MetaMap mm) <- M.lookup style m =
        let substInlineStyle' (Just (MetaBlocks
                                        [MBPLAIN (RawInline f s : r)])) =
                RawInline f $ substParams fm params $
                                s ++ stringifyInlines fm (map subst r)
                where params = (alt, tgt)
                      subst (Style "ALT") = RawInline f "$ALT$"
                      subst i = i
            substInlineStyle' Nothing = cons alt tgt
            substInlineStyle' _ = i
        in substInlineStyle' $ M.lookup fmt mm
substInlineStyle _ _ i = i

toInlineParams :: Inline -> Maybe (InlineParams, InlineCons)
toInlineParams (Image attr (style@(Style _) : Alt alt) tgt) =
    Just ((style, alt, tgt), Image attr)
toInlineParams (Link attr (style@(Style _) : Alt alt) tgt) =
    Just ((style, alt, tgt), Link attr)
toInlineParams _ = Nothing

substParams :: Format -> PureInlineParams -> String -> String
substParams fm (alt, (src, title)) s =
    foldr (\(p, is) -> replace p $ stringifyInlines fm is) s
          [("$ALT$", alt), ("$SRC$", [Str src]), ("$TITLE$", [Str title])]

stringifyInlines :: Format -> [Inline] -> String
stringifyInlines (Format fmt) p =
    let writer = getWriter fmt
        doc = Pandoc (Meta M.empty) [Plain p]
    in case writer of
        Left _ -> error $ "Unknown format " ++ fmt
#if MIN_VERSION_pandoc(2,0,0)
        Right (TextWriter w, _) ->
            case runPure $ w def doc of
                Left e -> displayException e
                Right r -> T.unpack r
        Right (ByteStringWriter w, _) ->
            case runPure $ w def doc of
                Left e -> displayException e
                Right r -> C8L.unpack r
#else
        Right (PureStringWriter w, _) -> w def doc
        _ -> error $ "Unsupported format " ++ fmt ++ ", try Pandoc 2.0!"
#endif

main :: IO ()
main = toJSONFilter styleFromMeta

