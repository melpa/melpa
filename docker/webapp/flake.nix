{
  description = "webapp";

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      forAllSystems = nixpkgs.lib.genAttrs nixpkgs.lib.platforms.all;
    in
    {
      devShell =
        forAllSystems (system:
          let
            pkgs = import nixpkgs { inherit system; };
          in
          pkgs.mkShell {
            buildInputs = [ pkgs.python3 pkgs.poetry ];
          }
        );
    };
}
