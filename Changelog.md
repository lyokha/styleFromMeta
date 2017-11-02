### 0.2.0.0

- Render substitutions using Pandoc's writers.
- Improved matching algorithms in metadata blocks.
- Implemented a new method of substitution using code blocks in metadata. This
  method must be preferred for all output formats.
- Added verbatim placeholders `$$SRC$$` and `$$TITLE$$`, and placeholder
  `$$ALT$$` in which all formatting is removed.

### 0.1.1.0

- Added support for Pandoc 2.0.
- Dropped support for GHC older than 7.8 (pattern synonyms are required).

