with import <nixpkgs> { };

haskell.lib.buildStackProject {
  name = "tom-hakyll";
  ghc = haskell.packages.ghc822.ghcWithPackages (ps : with ps; [ hakyll pandoc time process ]);
  buildInputs = [ pcre zlib ];
}
