{
  description = "Pure Common Lisp DEFLATE, zlib and gzip codecs.";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    cl-weave = { url = "github:nerima-lisp/cl-weave/v1.3.0"; inputs.nixpkgs.follows = "nixpkgs"; };
  };
  outputs = { self, nixpkgs, cl-weave, ... }:
    let systems = [ "aarch64-darwin" "x86_64-linux" ];
        each = f: nixpkgs.lib.genAttrs systems (system: f system (import nixpkgs { inherit system; }));
    in {
      formatter = each (system: pkgs: pkgs.nixfmt-tree);
      packages = each (system: pkgs: { default = pkgs.stdenvNoCC.mkDerivation {
        pname = "cl-deflate-kit"; version = "0.1.0"; src = self; dontBuild = true;
        installPhase = ''mkdir -p $out/share/common-lisp/source/cl-deflate-kit; cp -r *.asd src t README.md LICENSE $out/share/common-lisp/source/cl-deflate-kit/'';
        meta.license = pkgs.lib.licenses.mit;
      }; });
      checks = each (system: pkgs: {
        default = pkgs.stdenvNoCC.mkDerivation {
          pname = "cl-deflate-kit-check"; version = "0.1.0"; src = self;
          nativeBuildInputs = [ pkgs.sbcl cl-weave.packages.${system}.default pkgs.gzip ];
          buildPhase = ''
            export HOME="$TMPDIR/home"
            mkdir -p "$HOME"
            export CL_SOURCE_REGISTRY="$PWD//:${cl-weave.packages.${system}.default}/share/common-lisp/source//"
            sbcl --noinform --non-interactive \
              --eval '(require :asdf)' \
              --load cl-deflate-kit.asd \
              --eval '(asdf:test-system "cl-deflate-kit")'
          '';
          installPhase = ''touch $out'';
        };
      });
      devShells = each (system: pkgs: { default = pkgs.mkShell { packages = [ pkgs.sbcl cl-weave.packages.${system}.default pkgs.coreutils ]; }; });
      apps = each (system: pkgs: let test = pkgs.writeShellApplication { name = "cl-deflate-kit-test"; runtimeInputs = [ pkgs.sbcl cl-weave.packages.${system}.default ]; text = ''export CL_SOURCE_REGISTRY="$PWD//:${cl-weave.packages.${system}.default}/share/common-lisp/source//"; sbcl --noinform --non-interactive --eval '(require :asdf)' --eval '(asdf:test-system "cl-deflate-kit")' ''; }; in {
        default = { type = "app"; program = "${test}/bin/cl-deflate-kit-test"; };
        test = { type = "app"; program = "${test}/bin/cl-deflate-kit-test"; };
      });
    };
}
