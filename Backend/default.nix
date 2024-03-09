{ inputs, ... }:
{
  imports = [
    ./nix/debug.nix
    ./nix/docker.nix
    ./nix/scripts.nix
    ./nix/run-mobility-stack.nix
    ./nix/arion-configuration.nix
    ./nix/osrm.nix
    ./load-test
  ];
  perSystem = { config, self', pkgs, lib, system, ... }: {
    pre-commit.settings.imports = [
      ./nix/pre-commit.nix
    ];

    haskellProjects.default = {
      projectRoot = ./.;
      imports = [
        inputs.beckn-gateway.haskellFlakeProjectModules.output
        # inputs.namma-dsl.haskellFlakeProjectModules.output
        inputs.haskell-cac.haskellFlakeProjectModules.output
      ];
      autoWire = [ "packages" "checks" "apps" ];
      devShell.tools = _: {
        inherit (self'.packages)
          arion;
      };
      packages = {
        amazonka.source = inputs.amazonka-git + /lib/amazonka;
        amazonka-core.source = inputs.amazonka-git + /lib/amazonka-core;
        amazonka-test.source = inputs.amazonka-git + /lib/amazonka-test;
        amazonka-sso.source = inputs.amazonka-git + /lib/services/amazonka-sso;
        amazonka-sts.source = inputs.amazonka-git + /lib/services/amazonka-sts;
        amazonka-ses.source = inputs.amazonka-git + /lib/services/amazonka-ses;
        streamly.source = "0.8.3";
        unicode-data.source = "0.3.1";
        namma-dsl.source = inputs.namma-dsl + /lib/namma-dsl;
      };
      settings = {
        dynamic-offer-driver-app.custom = p: p.overrideAttrs (oa: {
          DYLD_LIBRARY_PATH = "${config.haskellProjects.default.outputs.finalPackages.cac_client}/lib";
        });
        namma-dsl.libraryProfiling = false;
        location-updates.check = false;
        beckn-test.check = false;
        singletons-th.jailbreak = true;
        singletons-base = {
          jailbreak = true;
          check = false;
        };
        streamly = {
          extraBuildDepends = lib.optional pkgs.stdenv.isDarwin pkgs.darwin.apple_sdk.frameworks.Cocoa;
          jailbreak = true;
        };
        base32.jailbreak = true;
        amazonka-core.check = false;
      };
    };

    packages = {
      # The final nammayatri package containing the various executables and
      # configuration files.
      nammayatri =
        let
          localCabalPackages = builtins.map
            (p: if p.exes != { } then lib.getBin p.package else null)
            (lib.attrValues config.haskellProjects.default.outputs.packages);
        in
        pkgs.symlinkJoin {
          name = "nammayatri";
          paths = localCabalPackages;
          postBuild = ''
            # Prepare /opt/app layout for Docker image.
            # Rationale: Our k8s deployment config is hardcoded to look for exes
            # under /opt/app.
            mkdir -p $out/opt/app
            for f in `${lib.getExe pkgs.fd} . $out/bin/`; do
              ln -s $f $out/opt/app/
            done
            cp -r ${./dhall-configs} $out/opt/app/dhall-configs
            cp -r ${./swagger} $out/opt/app/swagger
          '';
        };
    };

    devShells.backend = pkgs.mkShell {
      name = builtins.traceVerbose "devShells.backend" "ny-backend";
      meta.description = "Backend development environment for nammayatri";
      packages = with pkgs; [
        redis # redis-cli is used in scripts.nix
        jq
        gdal
        postgis
      ];
      # cf. https://haskell.flake.page/devshell#composing-devshells
      inputsFrom = [
        config.mission-control.devShell
        config.pre-commit.devShell
        config.haskellProjects.default.outputs.devShell
        config.flake-root.devShell
        inputs.haskell-cac.devShells.${system}.haskell-cac
      ];
      shellHook = ''
        export DYLD_LIBRARY_PATH="${config.haskellProjects.default.outputs.finalPackages.cac_client}/lib"
      '';
    };
  };
}
