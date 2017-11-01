### 0.2.0.0

- Render substitutions using Pandoc's writers.
- Improved matching algorithms in metadata blocks.
- Added verbatim placeholders `$$SRC$$` and `$$TITLE$$`.
- Put explicitly in docs that only HTML and LaTeX formats are supported (other
  formats are not guaranteed to work because Pandoc treats metadata blocks as
  parts of document hierarchy thus expecting in them either Markdown code or
  inlined HTML or TeX).

### 0.1.1.0

- Added support for Pandoc 2.0.
- Dropped support for GHC older than 7.8 (pattern synonyms are required).

