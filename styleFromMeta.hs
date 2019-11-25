{-# LANGUAGE ViewPatterns, PatternGuards, PatternSynonyms #-}

#if MIN_VERSION_pandoc_types(1,20,0)
{-# LANGUAGE OverloadedStrings #-}
#endif

import           Text.Pandoc.JSON
import           Text.Pandoc.Walk (walk)
import           Text.Pandoc.Options (def)
import           Text.Pandoc.Shared (stringify)
import qualified Data.Map as M
#if !MIN_VERSION_pandoc_types(1,20,0)
import           Data.String.Utils (replace)
#endif

#if MIN_VERSION_pandoc(2,0,0)
import           Text.Pandoc.Writers (Writer (..), getWriter)
import           Text.Pandoc.Class (PandocPure, runPure)
import           Text.Pandoc.Error (PandocError)
import qualified Data.ByteString.Lazy.Char8 as C8L
import qualified Data.Text as T
import           Control.Exception (displayException)
#else
import           Text.Pandoc (Writer (..), getWriter)
#endif

#if MIN_VERSION_pandoc_types(1,20,0)
type STRING = T.Text
rEPLACE :: STRING -> STRING -> STRING -> STRING
rEPLACE = T.replace
tOSTRING :: T.Text -> String
tOSTRING = T.unpack
fROMSTRING :: String -> T.Text
fROMSTRING = T.pack
tOTEXT :: T.Text -> T.Text
tOTEXT = id
#else
type STRING = String
rEPLACE :: STRING -> STRING -> STRING -> STRING
rEPLACE = replace
tOSTRING :: String -> String
tOSTRING = id
fROMSTRING :: String -> String
fROMSTRING = id
tOTEXT :: T.Text -> String
tOTEXT = T.unpack
#endif

#if MIN_VERSION_pandoc(2,8,0)
rUNWRITER :: PandocPure a -> Either PandocError a
rUNWRITER = runPure
#else
rUNWRITER :: (a -> b) -> a -> b
rUNWRITER = (id .)
#endif

#if MIN_TOOL_VERSION_ghc(7,10,1)
pattern Style :: STRING -> Inline
#endif
pattern Style x <- Math InlineMath x

#if MIN_TOOL_VERSION_ghc(7,10,1)
pattern Subst :: STRING -> Inline
#endif
pattern Subst x = Math InlineMath x

#if MIN_TOOL_VERSION_ghc(7,10,1)
pattern SubstVerbatim :: STRING -> Inline
#endif
pattern SubstVerbatim x <- Math DisplayMath x

#if MIN_TOOL_VERSION_ghc(7,10,1)
pattern Alt :: [Inline] -> [Inline]
#endif
pattern Alt x <- (dropWhile (== Space) -> x)

type MMap = M.Map STRING MetaValue
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
                    where substInlineStyle'' mb =
                            case mb of
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
toInlineParams (Image attr (style@(Style _) : Alt alt) tgt) =
    Just ((style, alt, tgt), Image attr)
toInlineParams (Link attr (style@(Style _) : Alt alt) tgt) =
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

substParamsInRawBlock :: Format -> PureInlineParams -> STRING -> STRING
substParamsInRawBlock fm (alt, (src, title)) s =
    foldr (\(p, is) -> rEPLACE p $ renderInlines fm is) s
          [("$ALT$",     alt                                           )
          ,("$SRC$",     [Str src]                                     )
          ,("$TITLE$",   [Str title]                                   )
          ,("$$ALT$$",   [RawInline fm $ stringify $ Span nullAttr alt])
          ,("$$SRC$$",   [RawInline fm src]                            )
          ,("$$TITLE$$", [RawInline fm title]                          )
          ]

renderBlocks :: Format -> [Block] -> STRING
renderBlocks fm p =
    let fmt = toWriterFormat fm
        writer = getWriter fmt
        doc = Pandoc (Meta M.empty) p
    in case rUNWRITER writer of
           Left _ -> error $ "Unknown format " ++ tOSTRING fmt
#if MIN_VERSION_pandoc(2,0,0)
           Right (TextWriter w, _) ->
               case runPure $ w def doc of
                   Left e -> fROMSTRING $ displayException e
                   Right r -> tOTEXT r
           Right (ByteStringWriter w, _) ->
               case runPure $ w def doc of
                   Left e -> fROMSTRING $ displayException e
                   Right r -> fROMSTRING $ C8L.unpack r
#else
           Right (PureStringWriter w) -> w def doc
           _ -> error $ "Unsupported format " ++ tOSTRING fmt ++
               ", use Pandoc 2.0 or newer!"
#endif

renderInlines :: Format -> [Inline] -> STRING
renderInlines fm p = renderBlocks fm [Plain p]

toWriterFormat :: Format -> STRING
toWriterFormat (Format "tex") = "latex"
toWriterFormat (Format fmt)   = fmt

main :: IO ()
main = toJSONFilter styleFromMeta

