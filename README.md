# Haslex Guard

[![Build Status](https://travis-ci.org/int-index/haslex-guard.svg?branch=master)](https://travis-ci.org/int-index/haslex-guard)

Verify your Haskell source files against exceptionally strict and opinionated
whitespace and formatting rules. For now, the rules are:

* the lines should not exceed 80 characters
* indentation should be exactly 2 spaces
* no trailing whitespace

# Usage

```
stack exec -- haslex-guard [FILENAME.hs]...
```
