{ pkgs ? import <nixpkgs> {} }:

with pkgs;

mkShell {
  buildInputs = [
    clojure
    leiningen
    clj-kondo
  ];
}
