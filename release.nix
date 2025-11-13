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
    nixGhc961 = (import ./nixpkgs { inherit system; }).haskell.packages.ghc961.override {
      overrides = self: super: {
        reflex = self.callHackageDirect {
          pkg = "reflex";
          ver = "0.9.3.0";
          sha256 = "1rs2fzgw5r6lw1bhhx0lgjqwd772qkgrqcq88agigbzx4ybrr28x";
        } {};

        reflex-process = pkgs.haskell.lib.doJailbreak super.reflex-process;

        vty = self.callHackageDirect {
          pkg = "vty";
          ver = "6.2";
          sha256 = "sha256-7NePMONSoKJ/DQa4FSsFBy8ScGwWHU0nL7DomJCh+9Y=";
        } {};
        vty-crossplatform = self.callHackageDirect {
          pkg = "vty-crossplatform";
          ver = "0.4.0.0";
          sha256 = "7b72e71b7a2e1a947f825a44a1fdea2e3e147944410c4286390aa4124a56358b";
        } {};
        vty-unix = self.callHackageDirect {
          pkg = "vty-unix";
          ver = "0.2.0.0";
          sha256 = "b03a315f1aa8f70e5e3aab36b88ed2e49cd646c56b1e34c195dae13c929ca926";
        } {};
        reflex-vty = self.callHackageDirect {
          pkg = "reflex-vty";
          ver = "0.6.2.0";
          sha256 = "077x2gil5wv76yw1yisksvd9ix307jccr1y0v1rxhd7z1ry1ira4";
        } {};

        patch = self.callHackageDirect {
          pkg = "patch";
          ver = "0.0.8.2";
          sha256 = "160zqqhjg48fr3a33gffd82qm3728c8hwf8sn37pbpv82fw71rzg";
        } {};

        these-lens = self.callHackageDirect {
          pkg = "these-lens";
          ver = "1.0.1.3";
          sha256 = "0n1vkr57jz5yvy4jm15v5cs42rp342ni0gisib7aqyhibpicqs5c";
        } {};
        these = self.callHackageDirect {
          pkg = "these";
          ver = "1.2";
          sha256 = "1iaaq1fsvg8c3l0czcicshkmbbr00hnwkdamjbkljsa1qvlilaf0";
        } {};
        lens = self.callHackageDirect {
          pkg = "lens";
          ver = "5.2.2";
          sha256 = "0c4a421sxfjm1cj3nvgwkr4glll23mqnsvs2iv5qh85931h2f3cy";
        } {};

        assoc = self.callHackageDirect {
          pkg = "assoc";
          ver = "1.1";
          sha256 = "1krvcafrbj98z5hv55gq4zb1in5yd71nmz9zdiqgnywjzbrvpf75";
        } {};

        strict = self.callHackageDirect {
          pkg = "strict";
          ver = "0.5";
          sha256 = "02iyvrr7nd7fnivz78lzdchy8zw1cghqj1qx2yzbbb9869h1mny7";
        } {};

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
    ghc961 = nixGhc961.callCabal2nix "vty-whisper" (import ./src.nix) {};
  })
