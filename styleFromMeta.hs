{-# LANGUAGE CPP, ViewPatterns, PatternSynonyms, LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import           Text.Pandoc.JSON
import           Text.Pandoc.Walk (walk)
import           Text.Pandoc.Options (WriterOptions (writerExtensions), def)
import           Text.Pandoc.Shared (stringify)
import           Text.Pandoc.Writers (Writer (..), getWriter)
import           Text.Pandoc.Class (runPure)
import           Text.Pandoc.Error (renderError)
#if MIN_VERSION_pandoc(3,0,0)
import           Text.Pandoc.Format (FlavoredFormat (..))
#endif
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as C8L
import qualified Data.Map as M

pattern Style :: Text -> Inline
pattern Style x <- Math InlineMath x

pattern Subst :: Text -> Inline
pattern Subst x = Math InlineMath x

pattern SubstVerbatim :: Text -> Inline
pattern SubstVerbatim x <- Math DisplayMath x

pattern Alt :: [Inline] -> [Inline]
pattern Alt x <- (dropWhile (== Space) -> x)

type MMap = M.Map Text MetaValue
type PureInlineParams = ([Inline], Target)          -- style:(alt, target)
type InlineParams = (Inline, [Inline], Target)      -- (style:alt, target)
type InlineCons = [Inline] -> Target -> Inline      -- Image or Link

#if MIN_VERSION_pandoc(3,0,0)
toWriterFormat :: Format -> FlavoredFormat
toWriterFormat (Format "tex") = FlavoredFormat "latex" mempty
toWriterFormat (Format fmt)   = FlavoredFormat fmt mempty
#else
toWriterFormat :: Format -> Text
toWriterFormat (Format "tex") = "latex"
toWriterFormat (Format fmt)   = fmt
#endif

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
substBlockStyle (Format fmt) m b@(Para cnt)
    | Just (MetaMap mm) <- M.lookup "para_style" m
    , Just (MetaBlocks [p]) <- M.lookup fmt mm =
        case p of
            Para  [Span attr _] -> Plain [Span attr cnt]
            Plain [Span attr _] -> Plain [Span attr cnt]
            _                   -> b
substBlockStyle _ _ b = b

substInlineStyle :: Format -> MMap -> Inline -> Inline
substInlineStyle fm@(Format fmt) m
                 i@(toInlineParams -> Just ((Style style, alt, tgt), cons))
    | Just (MetaMap mm) <- M.lookup style m =
        let params = (alt, tgt)
            substPlainParams = Span nullAttr . map (substParams fm params)
            substInlineStyle' (Just (MetaBlocks [CodeBlock _ vb])) =
                RawInline fm $ substParamsInRawBlock fm params vb
            substInlineStyle' (Just (MetaBlocks mbs)) =
                RawInline fm $ renderInlines fm $ map substInlineStyle'' mbs
                where substInlineStyle'' = \case
                          Plain is       -> substPlainParams is
                          Para is        -> substPlainParams is
                          d@Div {}       ->
                              RawInline fm $
                                  substParamsInRawBlock fm params $
                                      renderBlocks fm [d]
                          RawBlock bfm b ->
                              RawInline bfm $
                                  substParamsInRawBlock bfm params b
                          _              -> i
            substInlineStyle' Nothing = cons alt tgt
            substInlineStyle' _ = i
        in substInlineStyle' $ M.lookup fmt mm
substInlineStyle _ _ i = i

toInlineParams :: Inline -> Maybe (InlineParams, InlineCons)
toInlineParams (Image attr (style@Style {} : Alt alt) tgt) =
    Just ((style, alt, tgt), Image attr)
toInlineParams (Link attr (style@Style {} : Alt alt) tgt) =
    Just ((style, alt, tgt), Link attr)
toInlineParams _ = Nothing

substParams :: Format -> PureInlineParams -> Inline -> Inline
substParams _  (alt, _)        (Subst "ALT")           = Span nullAttr alt
substParams fm params          (SubstVerbatim "ALT")   = RawInline fm $
    stringify $ substParams fm params $ Subst "ALT"
substParams _  (_, (src, _))   (Subst "SRC")           = Str src
substParams fm (_, (src, _))   (SubstVerbatim "SRC")   = RawInline fm src
substParams _  (_, (_, title)) (Subst "TITLE")         = Str title
substParams fm (_, (_, title)) (SubstVerbatim "TITLE") = RawInline fm title
substParams _  params          (RawInline fm s)        = RawInline fm $
    substParamsInRawBlock fm params s
substParams _  _               i                       = i

substParamsInRawBlock :: Format -> PureInlineParams -> Text -> Text
substParamsInRawBlock fm (alt, (src, title)) s =
    foldr (\(p, is) -> T.replace p $ renderInlines fm is) s
          [("$ALT$",     alt                                           )
          ,("$SRC$",     [Str src]                                     )
          ,("$TITLE$",   [Str title]                                   )
          ,("$$ALT$$",   [RawInline fm $ stringify $ Span nullAttr alt])
          ,("$$SRC$$",   [RawInline fm src]                            )
          ,("$$TITLE$$", [RawInline fm title]                          )
          ]

renderBlocks :: Format -> [Block] -> Text
renderBlocks fm p =
    let writer = getWriter $ toWriterFormat fm
        doc = Pandoc (Meta M.empty) p
    in case runPure writer of
           Left e -> renderError e
           Right (TextWriter w, ext) ->
               case runPure $ w def {writerExtensions = ext} doc of
                   Left e -> renderError e
                   Right r -> r
           Right (ByteStringWriter w, ext) ->
               case runPure $ w def {writerExtensions = ext} doc of
                   Left e -> renderError e
                   Right r -> T.pack $ C8L.unpack r

renderInlines :: Format -> [Inline] -> Text
renderInlines fm p = renderBlocks fm [Plain p]

main :: IO ()
main = toJSONFilter styleFromMeta

