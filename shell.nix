with import <nixpkgs> { };

haskell.lib.buildStackProject {
  name = "tom-hakyll";
  ghc = haskell.packages.ghc7103.ghc;
  buildInputs = [ pcre zlib ];
}
