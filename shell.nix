{ sources ? import ./nix/sources.nix
, pkgs ? import ./nix/pkgs.nix { inherit sources; }
, pre-commit ? import ./nix/pre-commit.nix { inherit sources; }
}:
pkgs.haskell.lib.buildStackProject {
  name = "smos-nix-shell";
  buildInputs = with pkgs; [
    (import sources.niv { }).niv
    autoconf
    bzip2
    feedback
    git
    haskellPackages.autoexporter
    hub
    killall
    xlibsWrapper
    xorg.libXScrnSaver
    xorg.libXrandr
    xorg.libXrender
    zlib
  ] ++ pre-commit.tools;
  shellHook = pre-commit.run.shellHook + pkgs.feedback.shellHook;
}
