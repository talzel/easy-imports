# Changelog for easy-imports
0.1.0.1
- ~~update cabal file from `easy-imports` automatically~~ (added a flag, because of the speed using stack)
- updateCabal (updates cabal file from stack file automatically)

## Unreleased changes

## Ideas
- in order to tell from which package imports statements origin, we can
    - parse cabal file
    - get local modules. from them we can tell which imports are local to the package and ignore them
    - get dependencies - to limit the amount of knowledge we need for analyzing the package