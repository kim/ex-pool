# ex-pool

A fork of [resource-pool](http://hackage.haskell.org/package/resource-pool)
providing a high-performance striped resource pooling implementation for
Haskell.

The package is intended to be a drop-in replacement for the original
version, for when the following properties seem useful:

  * Unlike the original implementation, this library enables actions to be run
    in `MonadIO` (instead of plain `IO`), and uses
    [exceptions](http://hackage.haskell.org/package/exceptions) for exception
    handling.

  * Also, a number of yet-to-be-released enhancements and fixes to the original
    library are included (namely [#11](https://github.com/bos/pool/pull/11),
    [#15](https://github.com/bos/pool/pull/15),
    [#16](https://github.com/bos/pool/pull/16)).

That is, if you don't need any of the above, use the original package :)

# Contributing

Please report [issues here](https://github.com/kim/ex-pool/issues), or, better
yet, submit [pull requests](https://github.com/kim/ex-pool/pulls).

# License

BSD3, see `LICENSE` file
