# GHC User

This library attempts to make the GHC API much more user-friendly, using familiar type classes, avoiding dangerous operations, etc.

Check out the [Haddocks](https://sellout.github.io/ghc-user/index.html).

## Working with this project.

The Haskell is generated from the `ghc` library using a Perl script. After running the initial generation, you can use `cabal` to build, etc. None of the Haskell code is in version control. All changes must be made via the [generate-haskell.pl](./generate-haskell.pl) script and the files in [templates/](./templates).

```shell
./generate-haskell.pl
cabal build
cabal haddock
```

## Understanding the GHC API



## TODO

1. re-organize modules
   - An initial hierachy has been created that adds more structure than that in `ghc`, but it's not very well informed. I think there is still a lot of renaming and rearranging to be done.
2. restructure module internals
   a. don't re-export unsafe operations
   b. give clearer (unprefixed) names to things
   c. add lots of documentation
   d. remove custom monads
3. re-organize modules to be more user-centric
4. split project into multiple packages

## Open Questions

- should `GhcUser.Simplifier` be `GhcUser.Core.Simplifier`?
