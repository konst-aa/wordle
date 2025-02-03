{
  description = "wordle written in scheme";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }: let
    linuxPkgs = nixpkgs.legacyPackages.x86_64-linux;
    darwinPkgs = nixpkgs.legacyPackages.aarch64-darwin;
    deps = pkgs: (with pkgs.chickenPackages.chickenEggs; [
        pkgs.chicken
        pkgs.makeWrapper
        pkgs.gnumake
        sdl2
        sdl2-ttf
        srfi-13
        srfi-1
        srfi-18
        vector-lib
      ]);
    deriv = pkgs:
      pkgs.stdenv.mkDerivation {
      pname = "wordle";
      version = "0.0.0";
      src = ./.;
      buildInputs = deps pkgs;
      installPhase = ''
        mkdir -p $out/bin
        mkdir -p $out/etc

        cp tnr.ttf $out/etc
        cp dict-la.scm $out/etc
        cp dict-ta.scm $out/etc

        make

        cp wordle-multiplayer wordle wordle-server $out/bin/

        for f in $out/bin/*
        do 
          wrapProgram $f \
           --set CHICKEN_REPOSITORY_PATH $CHICKEN_REPOSITORY_PATH \
           --set WORDLE_ETC $out/etc/
        done
      '';
    };


  in {

    packages.x86_64-linux.wordle = deriv linuxPkgs;
    packages.aarch64-darwin.wordle = deriv darwinPkgs;

    packages.x86_64-linux.default = self.packages.x86_64-linux.wordle;
    packages.aarch64-darwin.default = self.packages.aarch64-darwin.wordle;


    devShells.x86_64-linux.default = linuxPkgs.mkShell {
      buildInputs = deps linuxPkgs;
    };
  };
}
