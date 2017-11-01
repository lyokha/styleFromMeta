{-# LANGUAGE ViewPatterns, PatternGuards, PatternSynonyms #-}

import           Text.Pandoc.JSON
import           Text.Pandoc.Walk (walk)
import           Text.Pandoc.Options (def)
import qualified Data.Map as M
import           Data.String.Utils (replace)

#if MIN_VERSION_pandoc(2,0,0)
import           Text.Pandoc.Writers (Writer (..), getWriter)
import           Text.Pandoc.Class (runPure)
import qualified Data.ByteString.Lazy.Char8 as C8L
import qualified Data.Text as T
import           Control.Exception (displayException)

#define MBPLAIN Plain
#else
import           Text.Pandoc (Writer (..), getWriter)

#define MBPLAIN Para
#endif

pattern Style :: String -> Inline
pattern Style x <- Math InlineMath x

pattern Subst :: String -> Inline
pattern Subst x <- Math InlineMath x

pattern SubstVerbatim :: String -> Inline
pattern SubstVerbatim x <- Math DisplayMath x

pattern Alt :: [Inline] -> [Inline]
pattern Alt x <- (dropWhile (== Space) -> x)

type MMap = M.Map String MetaValue
type PureInlineParams = ([Inline], Target)          -- style:(alt, target)
type InlineParams = (Inline, [Inline], Target)      -- (style:alt, target)
type InlineCons = [Inline] -> Target -> Inline      -- Image or Link

styleFromMeta :: Maybe Format -> Pandoc -> IO Pandoc
styleFromMeta (Just fm) (Pandoc m bs) = do
    let b = unMeta m
    return $ Pandoc m $
        walk (substInlineStyle fm b) $  -- apply styles for links and images
        walk (substBlockStyle fm b) bs  -- apply para_style to paragraphs
styleFromMeta _ p = return p

substBlockStyle :: Format -> MMap -> Block -> Block
substBlockStyle _ _ b@(Para is@(Image {} : _))
    | all isImage is = b    -- do not apply para_style to standalone images
    where isImage Image {} = True
          isImage _        = False
substBlockStyle (Format fmt) m (Para cnt)
    | Just (MetaMap mm) <- M.lookup "para_style" m
    , Just (MetaBlocks [MBPLAIN [Span attr _]]) <- M.lookup fmt mm =
        Plain [Span attr cnt]
substBlockStyle _ _ b = b

substInlineStyle :: Format -> MMap -> Inline -> Inline
substInlineStyle fm@(Format fmt) m
                 i@(toInlineParams -> Just ((Style style, alt, tgt), cons))
    | Just (MetaMap mm) <- M.lookup style m =
        let params = (alt, tgt)
            substPlainParams = Span nullAttr . map (substParams fm params)
            substInlineStyle' (Just (MetaBlocks mbs)) =
                RawInline fm $ renderInlines fm $ map substInlineStyle'' mbs
                    where substInlineStyle'' mb =
                            case mb of
                                Plain is      -> substPlainParams is
                                Para is       -> substPlainParams is
                                d@Div {}      ->
                                    RawInline fm $
                                        substParamsInRawBlock fm params $
                                            renderBlocks fm [d]
                                RawBlock bfm b ->
                                    RawInline bfm $
                                        substParamsInRawBlock bfm params b
                                _             -> i
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

substParams :: Format -> PureInlineParams -> Inline -> Inline
substParams _   (alt, _)        (Subst "ALT")           = Span nullAttr alt
substParams _   (_, (src, _))   (Subst "SRC")           = Str src
substParams fm  (_, (src, _))   (SubstVerbatim "SRC")   = RawInline fm src
substParams _   (_, (_, title)) (Subst "TITLE")         = Str title
substParams fm  (_, (_, title)) (SubstVerbatim "TITLE") = RawInline fm title
substParams _   params          (RawInline fm s)        = RawInline fm $
    substParamsInRawBlock fm params s
substParams _   _               i                       = i

substParamsInRawBlock :: Format -> PureInlineParams -> String -> String
substParamsInRawBlock fm (alt, (src, title)) s =
    foldr (\(p, is) -> replace p $ renderInlines fm is) s
          [("$ALT$",     alt                 )
          ,("$SRC$",     [Str src]           )
          ,("$TITLE$",   [Str title]         )
          ,("$$SRC$$",   [RawInline fm src]  )
          ,("$$TITLE$$", [RawInline fm title])
          ]

renderBlocks :: Format -> [Block] -> String
renderBlocks fm p =
    let fmt = toWriterFormat fm
        writer = getWriter fmt
        doc = Pandoc (Meta M.empty) p
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
        Right (PureStringWriter w) -> w def doc
        _ -> error $ "Unsupported format " ++ fmt ++ ", try Pandoc 2.0!"
#endif

renderInlines :: Format -> [Inline] -> String
renderInlines fm p = renderBlocks fm [Plain p]

toWriterFormat :: Format -> String
toWriterFormat (Format "tex") = "latex"
toWriterFormat (Format fmt)   = fmt

main :: IO ()
main = toJSONFilter styleFromMeta

