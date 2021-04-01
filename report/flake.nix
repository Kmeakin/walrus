{
  description = "Walrus language report";
  # Provides abstraction to boiler-code when specifying multi-platform outputs.
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = nixpkgs.legacyPackages.${system};
      in
        {
          devShell = pkgs.mkShell {
            nativeBuildInputs = with pkgs; [
              (rWrapper.override { packages = with rPackages; [ rmarkdown ]; })
              pandoc
              texlive.combined.scheme-full
            ];
          };
        }
    );
}
