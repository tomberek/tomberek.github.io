# Tom Hakyll

Used [this configuration](www.stackbuilders.com/news/dr-hakyll-create-a-github-page-with-hakyll-and-circleci) to integrate with CircleCI.

```
$ git clone https://github.com/stackbuilders/dr-hakyll
```

```
$ cabal sandbox init
$ cabal install --only-dependencies
$ cabal configure
$ cabal build
$ cabal run build
$ cabal run watch
```

## References

### Hakyll and Haskell

- [Hakyll][hakyll]
- [Haskell][haskell]

[hakyll]: http://jaspervdj.be/hakyll/
[haskell]: https://www.haskell.org/

### Bootstrap

- [Bootstrap][bootstrap]
- [Bootstrap's blog example][bootstrap-blog]

[bootstrap]: http://getbootstrap.com/
[bootstrap-blog]: http://getbootstrap.com/examples/blog/
