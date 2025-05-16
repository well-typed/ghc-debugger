{ pkgs ? import <nixpkgs> {} }:

pkgs.buildNpmPackage {
    pname = "haskell-debugger-extension";
    version = "0.3.0";
    src = ./.;

    npmDepsHash = "sha256-Sp5CCtw5kC82gjWL7x7aZ48+geJ/R+LJFMw8RkqsH6c=";

    nativeBuildInputs = [
      pkgs.nodejs pkgs.vsce pkgs.cacert pkgs.nodePackages.rimraf
      pkgs.esbuild pkgs.pkg-config
    ];

    buildInputs = [
      pkgs.libsecret.dev # needed for pkg-config for some npm thing
    ];

    buildPhase = ''
      npm run package
    '';

    installPhase = ''
      mkdir -p $out
      mv *.vsix $out/
    '';
}
