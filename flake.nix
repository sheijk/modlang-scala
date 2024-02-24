{
  description = "Build and dev environment for modlang";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    sbt.url = "github:zaninime/sbt-derivation";
    sbt.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    sbt
  }: let
    mainClass = "me.modlang.Main";

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

        java = pkgs.pkgs21.jdk11_headless;
        escapeShellArg = pkgs.pkgs21.lib.escapeShellArg;

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

        packages = rec {
          # modlang = sbt.mkSbtDerivation.${system} {
          #   pname = "modlang";
          #   # pkgs = nixpkgs.legacyPackages.${system};
          #   pkgs = "blah";
          #   src = ./.;
          #   version = "0.1";
          #   depsSha256 = "";
          # };
          modlang = sbt.mkSbtDerivation.aarch64-darwin rec {
            pname = "modlang";
            src = ./.;
            version = "0.1";
            depsSha256 = "sha256-5u5hnhBDOO+z8B2dUdgikLTx+ULqlahpYTWuhL1Qf5w=";
            # ...see below for all parameters
            # build your software with sbt, consider running unit tests too!
            buildPhase = ''
              sbt compile
              sbt stage
            '';

            installPhase = ''
              mkdir -p $out/{bin,lib}
              cp $(find . -ipath './target/scala-*/modlang_*.jar') $out/modlang.jar
              mkdir -p $out/{bin,lib}
              cp -ar target/universal/stage/lib $out/lib/${pname}
              makeWrapper ${java}/bin/java $out/bin/${pname} \
                --add-flags "-cp '$out/lib/${pname}/*' ${escapeShellArg mainClass}"
            '';

            runPhase = ''
              java -cp modlang.jar me.modlang.Main
            '';
          };

          default = modlang;
        };
      }
    );
}
