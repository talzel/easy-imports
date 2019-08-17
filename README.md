# easy-imports
This is a CLI tool for haskell projects whose goal is to simplify imports
management for haskell files.

warnings:
- it is suggested to use it in conjunction with git, because it might corrupt files

## Setup

`stack` is needed in order to build the tool:

``` bash
$ git checkout https://github.com/talzel/easy-imports.git
$ cd easy-imports
$ stack install --fast 
```

## Usage

updates package file with all packages imported in the package's modules.

```
easy-imports --packageDirectory <path/to/haskell/package> 
```

debugging:
```
easy-imports --packageDirectory <path to haskell package> --debug 
```


## examples:

```
$ # go to easy-imports folder
$ git status 
On branch master
Your branch is up to date with 'origin/master'.
$ easy-imports --packageDirectory <path/to/easy-imports/>
$ git status
On branch master
Your branch is up to date with 'origin/master'.

Changes not staged for commit:
  (use "git add <file>..." to update what will be committed)
  (use "git checkout -- <file>..." to discard changes in working directory)

        modified:   package.yaml
$ cat package.
...
```

warning: currently oprerates on `package.yaml` only. `stack` is needed to be run after
`easy-imports` to update the cabal file


## Roadmap
- ~~update cabal file from `easy-imports` automatically~~ (added a flag, because of the speed using stack)
- proper help flag information
- change examples to run on test files and not on the actual folder
- add flag for directories with multiple packages
- add warnings to unused imports in easy-imports output
- literate haskell support
- direct cabal support (eliminates the need for cabal update flag)
- nix installation
- cabal installation
- add test to run
- modify import statements to their PackageImports extension form by a fixed dict
- allow the option to override dict from external source
- update module with top level function names
- TBD: qualify an import in a module and qualify all items from that import
- TBD: solve clashing names (list vs set) via automated qualification
- TBD: help with compiler related warnings
