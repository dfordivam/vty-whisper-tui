{ reflex-platform ? import ./reflex-platform
}:
let
  pkgs = (reflex-platform {}).nixpkgs;
  supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
  inherit (pkgs) lib;
  # commonOverrides = self: super: {
  #   reflex-process = pkgs.haskell.lib.doJailbreak super.reflex-process;
  #   reflex-vty = self.callHackageDirect {
  #     pkg = "reflex-vty";
  #     ver = "0.4.1.0";
  #     sha256 = "sha256-R42Z9itpAgtTYqo/Q7doBn2HlMj7qKennuCQ4lq4w9o=";
  #   } {};
  #   vty = self.callHackageDirect {
  #     pkg = "vty";
  #     ver = "5.37";
  #     sha256 = "01ppspii5x02iik9rxkf2idi6r9ngax6w3mqi6d8qj765w3cksiq";
  #   } {};
  # };
in
  lib.genAttrs supportedSystems (system: let
    rp = reflex-platform { inherit system; __useNewerCompiler = true; };
    rpGhc = rp.ghc;
    haskellLib = pkgs.haskell.lib;
    nixGhc967 = (import ./nixpkgs { inherit system; }).haskell.packages.ghc967.override {
      overrides = self: super: {
        reflex-process = pkgs.haskell.lib.doJailbreak super.reflex-process;

        reflex-vty = haskellLib.doJailbreak (self.callHackageDirect {
          pkg = "reflex-vty";
          ver = "0.6.2.0";
          sha256 = "077x2gil5wv76yw1yisksvd9ix307jccr1y0v1rxhd7z1ry1ira4";
        } {});

        patch = haskellLib.doJailbreak (self.callHackageDirect {
          pkg = "patch";
          ver = "0.0.8.2";
          sha256 = "160zqqhjg48fr3a33gffd82qm3728c8hwf8sn37pbpv82fw71rzg";
        } {});


        # Jailbroken until https://github.com/audreyt/string-qq/pull/3
        string-qq = haskellLib.dontCheck super.string-qq;
        # Tests aren't compatible with transformers-0.6
        exception-transformers = haskellLib.doJailbreak (haskellLib.dontCheck super.exception-transformers);
      };
    };
  in
  {
    recurseForDerivations = true;
    ghc810 = rpGhc.callCabal2nix "vty-whisper" (import ./src.nix) {};
    ghc967 = nixGhc967.callCabal2nix "vty-whisper" (import ./src.nix) {};
  })
