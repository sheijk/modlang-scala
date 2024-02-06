{
  description = "Build and dev environment for modlang";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }: let
    supportedSystems = [
      "aarch64-darwin"
      "aarch64-linux"
      "x86_64-linux"
      "x86_64-darwin"
    ];
  in
    flake-utils.lib.eachSystem supportedSystems (
      system: let
        pkgs = import ./pkgs.nix nixpkgs system;

        makeShell = p:
          p.mkShell {
            buildInputs = with p; [
              ammonite
              bloop
              coursier
              jdk
              mill
              sbt
              scala-cli
              scalafmt
              metals
            ];
          };
      in {
        devShells = {
          default = makeShell pkgs.default;
          java21 = makeShell pkgs.pkgs21;
          java17 = makeShell pkgs.pkgs17;
          java11 = makeShell pkgs.pkgs11;
          java8 = makeShell pkgs.pkgs8;
        };

        formatter = pkgs.default.alejandra;
      }
    );
}

# {
#   description = "My Scala project";
# 
#   # you probably have this one already
#   inputs.nixpkgs.url = "github:NixOS/nixpkgs";
# 
#   # add this line
#   inputs.sbt.url = "github:zaninime/sbt-derivation";
#   # recommended for first style of usage documented below, but not necessary
#   inputs.sbt.inputs.nixpkgs.follows = "nixpkgs";
# 
#   outputs = {
#     self,
#     nixpkgs,
#     sbt,
#   }: {
#     # first style of usage
#     packages.aarch64-darwin = rec {
#       modlang = sbt.mkSbtDerivation.x86_64-linux {
#         pname = "modlang";
#         src = ./.;
#         version = "0.1";
#         depsSha256 = "";
#   # ...see below for all parameters
#       };
# 
#       default = modlang;
#     };
# 
#     # # second style of usage
#     # packages.x86_64-linux.my-second-scala-package = sbt.lib.mkSbtDerivation {
#     #   # pass your pkgs here
#     #   pkgs = nixpkgs.legacyPackages.x86_64-linux;
#     # 
#     #   # ...and the rest of the arguments
#     #   pname = "my-scala-package";
#     # };
#   };
# }
