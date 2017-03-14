# Haslex Guard

Verify your Haskell source files against exceptionally strict and opinionated
whitespace and formatting rules. For now, the rules are:

* the lines should not exceed 80 characters
* indentation should be exactly 2 spaces
* no trailing whitespace

# Usage

```
stack exec -- haslex-guard --src_path FILENAME.hs
```
