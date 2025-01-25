{ system ? builtins.currentSystem or "x86_64-linux"
, ghc ? "ghc947"
}:

let
  nix = import ./nix;
  pkgs = nix.pkgSetForSystem system {
    config = {
      allowBroken = true;
      allowUnfree = true;
    };
  };
  inherit (pkgs) lib;
  hsPkgSetOverlay = pkgs.callPackage ./nix/haskell/overlay.nix {
    inherit (nix) sources;
  };

  sources = [
    "^src.*$"
    "^test.*$"
    "^.*\\.cabal$"
  ];

  base = hsPkgs.callCabal2nix "th-utilities" (lib.sourceByRegex ./. sources) { };
  th-utilities-overlay = _hf: _hp: { th-utilities = base; };
  baseHaskellPkgs = pkgs.haskell.packages.${ghc};
  hsOverlays = [ hsPkgSetOverlay th-utilities-overlay ];
  hsPkgs = baseHaskellPkgs.override (old: {
    overrides =
      builtins.foldl' pkgs.lib.composeExtensions (old.overrides or (_: _: { }))
      hsOverlays;
  });

  hls = pkgs.haskell.lib.overrideCabal hsPkgs.haskell-language-server
    (_: { enableSharedExecutables = true; });

  shell = hsPkgs.shellFor {
    packages = p: [ p.th-utilities ];
    nativeBuildInputs = (with pkgs; [
      cabal-install
      ghcid
      hlint
      hpack
      niv
    ]) ++ [ hls ];
    shellHook = ''
      export PS1='$ '
      hpack
    '';
  };

  th-utilities = hsPkgs.th-utilities;
in {
  inherit hsPkgs;
  inherit ghc;
  inherit pkgs;
  inherit shell;
  inherit th-utilities;
}
