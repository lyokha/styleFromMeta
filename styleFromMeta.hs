-- styleFromMeta.hs

{-# OPTIONS_HADDOCK prune, ignore-exports #-}

import Text.Pandoc.JSON
import Text.Pandoc.Walk (walk)
import Text.Pandoc.Shared (triml, stringify)
import qualified Data.Map as M
import Data.String.Utils (replace)

type MMap = M.Map String MetaValue
type ObjParams = ([Inline], String, String)

-- | Applies style found in the metadata of the document for various objects
--
-- Styling is supported for following types of objects:
--
--  * Standalone images
--
--  * Inline images
--
--  * Links
--
--  * Paragraphs (with restrictions, see below)
--
-- Styles are read from the metadata of the document: they may reside inside
-- the document or in a separate YAML file. For example
--
-- > ---
-- > img_style :
-- >   html : |
-- >     <div style="clear: both; text-align: center; margin-bottom: 16px">
-- >     <a href="$SRC$" style="margin-left: 10em;" alt="$ALT$">
-- >     <img border="0" src="$SRC$" /></a></div>
-- >   latex : |
-- >     \begin{center}
-- >     \includegraphics{$SRC$}
-- >     \end{center}
-- > link_style :
-- >   html : |
-- >     <a href="$SRC$" style="margin-left: 1em; margin-right: 1em;">$ALT$</a>
-- >   latex : |
-- >     \href{$SRC$}{\colorbox{green}{$ALT$}}
-- > para_style :
-- >   html : |
-- >     <span style="display: block; margin-bottom: 16px;"></span>
-- > ...
--
-- declares styles /img_style/, /link_style/ and /para_style/. Their names
-- (except for the last) are arbitrarily chosen and may be referred from the
-- document, for example
--
-- > ![$img_style$](../images/an_image.png)
-- > [$link_style$ *here*](http://example.com/)
--
-- Placeholders /$ALT$/, /$SRC$/ and /$TITLE$/ from style declarations are
-- to be replaced by concrete data found in the object declaration. In the
-- last example @*here*@ corresponds to /$ALT$/ and @http:\/\/example.com/@
-- corresponds to /$SRC$/.
--
-- As soon as paragraphs do not have place where to put extra data, style
-- /para_style/ is applied to all paragraphs in the document. Currently only
-- transformation to a span block is supported. Any contents found between
-- opening and closing span tags are ignored: actual paragraph contents will
-- be inserted inside them.
--
styleFromMeta :: Maybe Format -> Pandoc -> IO Pandoc
styleFromMeta (Just fm) p@(Pandoc m bs) =
    return $ Pandoc m (walk (substStyle fm (unMeta m)) bs)
styleFromMeta _ p = return p

substStyle :: Format -> MMap -> Block -> Block
substStyle (Format fm) m
           b@(Para [Image ((Math InlineMath style):alt) (src, title)]) =
    case M.lookup style m of
        Nothing -> b
        Just (MetaMap mm) ->
            let params = (alt, src, title)
            in case M.lookup fm mm of
                   Nothing -> b
                   Just (MetaBlocks [RawBlock f s]) ->
                       RawBlock f (substParams fm params s)
                   Just (MetaBlocks [mb]) ->
                       walk substParams' mb
                       where substParams' (RawInline f s) =
                                   RawInline f (substParams fm params s)
                             substParams' i = i
                   Just _ -> b
        Just _ -> b
substStyle (Format fm) m b@(Para cnt) =
    let walk' = walk (substInlineStyle (Format fm) m)
    in case M.lookup "para_style" m of
           Nothing -> walk' b
           Just (MetaMap mm) ->
               case M.lookup fm mm of
                   Nothing -> walk' b
                   Just (MetaBlocks [Para [Span attr _]]) ->
                       walk' (Plain [Span attr cnt])
                   Just _ -> walk' b
           Just _ -> walk' b
substStyle fm m b = walk (substInlineStyle fm m) b

substInlineStyle :: Format -> MMap -> Inline -> Inline
substInlineStyle fm m i@(Image ((Math InlineMath style):alt) (src, title)) =
    substInlineStyle' fm m style (alt, src, title) i
substInlineStyle fm m i@(Link ((Math InlineMath style):alt) (src, title)) =
    substInlineStyle' fm m style (alt, src, title) i
substInlineStyle _ _ i = i

substInlineStyle' :: Format -> MMap -> String -> ObjParams -> Inline -> Inline
substInlineStyle' (Format fm) m style params i =
    case M.lookup style m of
        Nothing -> i
        Just (MetaMap mm) ->
            case M.lookup fm mm of
                Nothing -> i
                Just (MetaBlocks [Para ((RawInline f s):r)]) ->
                        RawInline f (substParams fm params
                                        (s ++ (stringify' fm (map subst r))))
                    where subst (Math InlineMath "ALT") = RawInline f "$ALT$"
                          subst i = i
                Just _ -> i
        Just _ -> i

substParams :: String -> ObjParams -> String -> String
substParams fm (alt, src, title) s =
    foldr (\(a, b) -> replace a b) s
          [("$ALT$", triml . stringify' fm $ alt),
           ("$SRC$", src), ("$TITLE$", title)]

stringify' :: String -> [Inline] -> String
stringify' fm@("latex") =
    foldr ((++) . subst) ""
    where subst (Emph x) = "\\emph{" ++ (stringify' fm x) ++ "}"
          subst (Strong x) = "\\textbf{" ++ (stringify' fm x) ++ "}"
          subst (Strikeout x) = "\\sout{" ++ (stringify' fm x) ++ "}"
          subst (Superscript x) = "\\textsuperscript{" ++ (stringify' fm x) ++
                                  "}"
          subst (Subscript x) = "\\textsubscript{" ++ (stringify' fm x) ++ "}"
          subst (RawInline fm x) = x
          subst (Math _ x) = "$" ++ x ++ "$"
          subst x = stringify x
stringify' fm@("html") =
    foldr ((++) . subst) ""
    where subst (Emph x) = "<em>" ++ (stringify' fm x) ++ "</em>"
          subst (Strong x) = "<strong>" ++ (stringify' fm x) ++ "</strong>"
          subst (Strikeout x) = "<del>" ++ (stringify' fm x) ++ "</del>"
          subst (Superscript x) = "<sup>" ++ (stringify' fm x) ++ "</sup>"
          subst (Subscript x) = "<sub>" ++ (stringify' fm x) ++ "</sub>"
          subst (RawInline fm x) = x
          subst x = stringify x
stringify' _ = stringify

main :: IO ()
main = toJSONFilter styleFromMeta

