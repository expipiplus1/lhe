{ nixpkgsSrc ? <nixpkgs>, pkgs ? import nixpkgsSrc { }, compiler ? null
, extraOverrides ? _: _: { }, modifier ? x: x }:

let
  haskellPackages = if compiler == null then
    pkgs.haskellPackages
  else
    pkgs.haskell.packages.${compiler};

in haskellPackages.developPackage {
  name = "";
  root = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
  overrides = with pkgs.haskell.lib;
    pkgs.lib.composeExtensions (_self: _super: { }) extraOverrides;
  inherit modifier;
}
