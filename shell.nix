# shell.nix
let
  commitHash = "78dc359abf8217da2499a6da0fcf624b139d7ac3";
  tarballUrl = "https://github.com/NixOS/nixpkgs/archive/${commitHash}.tar.gz";
  pkgs = import (fetchTarball tarballUrl) {};
  stdenv = pkgs.stdenv;
in with pkgs; {
  myProject = stdenv.mkDerivation {
    name = "bdscod-r-env";
    version = "1";
    src = if pkgs.lib.inNixShell then null else nix;

    buildInputs = with rPackages; [
      R
      dplyr
      reshape2
      purrr
      magrittr
      ggplot2
      cowplot
      stringr
      jsonlite
      coda
    ];
   shellHook = ''
             printf "\n\nWelcome to a reproducible R shell :)\n\n"
      '';
  };
}
