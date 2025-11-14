{ enableVulkan ? false
, buildPackage ? true
}:

let
  # Use the local nixpkgs
  nixpkgs = import ./nixpkgs {};

  # Get the vty-whisper package from release.nix if needed
  release = import ./release.nix {};
  vty-whisper = release.${nixpkgs.system}.ghc967;

  # Select whisper-cpp variant based on Vulkan flag
  whisper-cpp = if enableVulkan then
    nixpkgs.whisper-cpp-vulkan
  else
    nixpkgs.whisper-cpp;

  # Build the package list
  packages = [
    whisper-cpp
  ] ++ nixpkgs.lib.optional buildPackage vty-whisper;

in
  nixpkgs.mkShell {
    buildInputs = packages;

    shellHook = ''
      ${if enableVulkan then ''
      export VTY_WHISPER_USE_VULKAN=true
      '' else ""}
      echo "Whisper transcription environment loaded"
      echo "- whisper-cpp: ${if enableVulkan then "with Vulkan support" else "CPU only"}"
      ${if buildPackage then ''
      echo "- vty-whisper: built and available in PATH"
      '' else ""}
      echo ""
      echo "Environment variables you can set:"
      echo "  VTY_WHISPER_MODEL - Path to the whisper model file (required)"
      echo "  VTY_WHISPER_THREADS - Number of threads to use (default: 8)"
      echo "  VTY_WHISPER_VERBOSE - Enable verbose output (default: false)"
      ${if enableVulkan then ''
      echo "  VTY_WHISPER_USE_VULKAN - Set to true (auto-set by nix-shell)"
      '' else ""}
      echo ""
    '';
  }
