### 0.4.0.0

- Dropped support for GHC older than *8.0* and *Pandoc* older than *2.8*.
- Allow Pandoc extensions available for the output format.

### 0.3.0.0

- Compatibility with Pandoc *3.0*.

### 0.2.4.0

- More granular Cabal build plan which lets skipping unneeded dependencies.

### 0.2.3.0

- Migrated from *MissingH* to *extra* for GHC *8.8.1* support.

### 0.2.2.0

- Updated after Pandoc *2.8* and the *String-to-Text* migration.

### 0.2.1.0

- Ensure that *para_style* is always found (different versions of Pandoc may
  render contents of *MetaBlocks* as *Para* or *Plain*).

### 0.2.0.0

- Render substitutions using Pandoc's writers.
- Improved matching algorithms in metadata blocks.
- Implemented a new method of substitution using code blocks in metadata. This
  method must be preferred for all output formats.
- Added verbatim placeholders `$$SRC$$` and `$$TITLE$$`, and placeholder
  `$$ALT$$` in which all formatting is removed.

### 0.1.1.0

- Added support for Pandoc 2.0.
- Dropped support for GHC older than *7.8* (pattern synonyms are required).

