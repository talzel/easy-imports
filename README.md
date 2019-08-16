# easy-imports

This is a CLI tool for haskell projects whose goal is to simplify imports
management for haskell files.

main features:
1. updates cabal file with all packages imported in the package's modules.
    - remove unused imports
    - TBA: literate haskell support, stack support
2. lift import statements to their PackageImports extension form
3. qualify import and qualify all calls from that import
4. update module with top level function names
5. solve clashing names (list vs set) 
