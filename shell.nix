(import (
  fetchTarball {
    url = "https://github.com/edolstra/flake-compat/archive/99f1c2157fba4bfe6211a321fd0ee43199025dbf.tar.gz";
    sha256 = "0x2jn3vrawwv9xp15674wjz9pixwjyj3j771izayl962zziivbx2"; }
) {
  src =  ./.;
}).shellNix
#with import ./nix { };
#(plutus-apps.plutus-apps.haskell.project.shellFor ({
#
#  # Select packages who's dependencies should be added to the shell env
#  packages = ps: [ ];
#
#  # Select packages which should be added to the shell env, with their dependencies
#  # Should try and get the extra cardano dependencies in here...
#  additional = ps:
#    with ps; [
#      plutus-tx
#      plutus-tx-plugin
#      plutus-ledger-api
#      pab.plutus_ledger_with_docs
#      plutus-core
#      plutus-contract
#      prettyprinter-configurable
#      cabal-doctest
#    ];
#
#  withHoogle = true;
#
#  # Extra haskell tools (arg passed on to mkDerivation)
#  # Using the plutus.pkgs to use nixpkgs version from plutus (nixpkgs-unstable, mostly)
#  propagatedBuildInputs = with pkgs; [
#    # Haskell Tools
#    stack
#    cabal-install
#    haskellPackages.fourmolu
#    entr
#    git
#    ghc
#    nixfmt
#    plutus.plutus.hlint
#
#    plutus.plutus.haskell-language-server
#
#    # hls doesn't support preprocessors yet so this has to exist in PATH
#    haskellPackages.record-dot-preprocessor
#
#    # Graphviz Diagrams for documentation
#    graphviz
#
#    ### Example contracts
#    #plutus-apps.plutus-pab-examples
#  ];
#
#  buildInputs = (with plutus-apps.pkgs;
#    [ zlib pkg-config libsodium-vrf R ]
#    ++ (lib.optionals (!stdenv.isDarwin) [ systemd ]));
#
#}))
