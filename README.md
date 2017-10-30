styleFromMeta
=============

Pandoc filter to apply styles found in the metadata of the document for various
objects.

Styling is supported for following types of objects:

- Standalone images
- Inline images
- Links
- Paragraphs (with restrictions, see below)

Styles are read from the metadata of the document: they may reside inside the
document or in a separate YAML file. For example

```yaml
    ---
    img_style :
      html : |
        <div style="clear: both; text-align: center; margin-bottom: 16px">
        <a href="$SRC$" style="margin-left: 10em;" alt="$ALT$">
        <img border="0" src="$SRC$" /></a></div>
      latex : |
        \begin{center}
        \includegraphics{$SRC$}
        \end{center}
    link_style :
      html : |
        <a href="$SRC$" style="margin-left: 1em; margin-right: 1em;">$ALT$</a>
      latex : |
        \href{$SRC$}{\colorbox{green}{$ALT$}}
    para_style :
      html : |
        <span style="display: block; margin-bottom: 16px;"></span>
    ...
```

declares styles *img\_style*, *link\_style* and *para\_style*. Their names
(except for the last) are arbitrarily chosen and may be referred from the
document, for example

```markdown
    ![$img_style$](../images/an_image.png)
    [$link_style$ *here*](http://example.com/)
```

Placeholders `$ALT$`, `$SRC$` and `$TITLE$` from style declarations are to be
replaced by concrete data found in the object declaration. In the last example
`*here*` corresponds to `$ALT$` and `http://example.com/` corresponds to
`$SRC$`.

As soon as paragraphs do not have place where to put extra data, style
*para\_style* is applied to all paragraphs in the document. Currently only
transformation to a span block is supported. Any contents found between opening
and closing span tags are ignored: actual paragraph contents will be inserted
inside them.

