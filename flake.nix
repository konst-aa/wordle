{
  description = "wordle written in scheme";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }: let
    pkgs = nixpkgs.legacyPackages.x86_64-linux;
    deps = with pkgs.chickenPackages.chickenEggs; [
        pkgs.chicken
        pkgs.makeWrapper
        sdl2
        sdl2-ttf
        srfi-13
        srfi-1
        srfi-18
        vector-lib
      ];

  in {

    packages.x86_64-linux.wordle = pkgs.stdenv.mkDerivation {
      pname = "wordle";
      version = "0.0.0";
      src = ./.;
      buildInputs = deps;
      installPhase = ''
        mkdir -p $out/bin
        mkdir -p $out/etc

        cp tnr.ttf $out/etc
        cp dict-la.scm $out/etc
        cp dict-ta.scm $out/etc

        # don't print banner and warnings
        echo "#!${pkgs.chicken}/bin/csi -qw" > $out/bin/wordle
        chmod +x $out/bin/wordle
        cat main.scm >> $out/bin/wordle

        for f in $out/bin/*
        do 
          wrapProgram $f \
           --set CHICKEN_REPOSITORY_PATH $CHICKEN_REPOSITORY_PATH \
           --set WORDLE_ETC $out/etc/
        done
      '';
    };
    packages.aarch64-darwin.wordle = self.packages.x86_64-linux.wordle;

    packages.x86_64-linux.default = self.packages.x86_64-linux.wordle;
    packages.aarch64-darwin.default = self.packages.aarch64-darwin.wordle;
    devShells.x86_64-linux.default = pkgs.mkShell {
      buildInputs = deps;
    };
  };
}
