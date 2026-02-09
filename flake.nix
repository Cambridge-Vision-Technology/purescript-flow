{
  description = "purescript-flow - Platform-agnostic workflow composition library for PureScript";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    purs-nix.url = "https://flakehub.com/f/Cambridge-Vision-Technology/purs-nix/0.1.tar.gz";
    purs-nix.inputs.nixpkgs.follows = "nixpkgs";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";

    # CVT linting tools
    purescript-whine = {
      url = "https://flakehub.com/f/Cambridge-Vision-Technology/purescript-whine/0.1.tar.gz";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        purs-nix.follows = "purs-nix";
      };
    };

    # PureScript duplicate code detector
    purescript-dedup = {
      url = "https://flakehub.com/f/Cambridge-Vision-Technology/purescript-dedup/*";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.purs-nix.follows = "purs-nix";
    };

    # PureScript dead code analyzer
    purescript-scythe = {
      url = "https://flakehub.com/f/Cambridge-Vision-Technology/purescript-scythe/*";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.purs-nix.follows = "purs-nix";
    };

    # PureScript library function finder
    purescript-drop = {
      url = "https://flakehub.com/f/Cambridge-Vision-Technology/purescript-drop/*";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.purs-nix.follows = "purs-nix";
    };

    # PureScript toolchain overlay (provides purs-tidy)
    purescript-overlay = {
      url = "github:thomashoneyman/purescript-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # CLAUDE.md generation
    agen = {
      url = "https://flakehub.com/f/Cambridge-Vision-Technology/agen/0.1.tar.gz";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{
      flake-parts,
      purs-nix,
      treefmt-nix,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        treefmt-nix.flakeModule
      ];

      systems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];

      perSystem =
        {
          config,
          system,
          inputs',
          ...
        }:
        let
          pkgs = import inputs.nixpkgs {
            inherit system;
            overlays = [ inputs.purescript-overlay.overlays.default ];
          };

          purs-nix-lib = purs-nix { inherit system; };

          baseDeps = [
            "arrays"
            "effect"
            "either"
            "exists"
            "foldable-traversable"
            "integers"
            "maybe"
            "newtype"
            "prelude"
            "profunctor"
            "strings"
            "tuples"
            "variant"
          ];

          ps = purs-nix-lib.purs {
            dependencies = baseDeps;
            srcs = [ ./src ];
            compile = {
              compilerOptions = [ "--json-errors" ];
            };
          };

          psWithTests = purs-nix-lib.purs {
            dependencies = baseDeps ++ [
              "aff"
              "identity"
              "spec"
              "spec-node"
            ];
            srcs = [
              ./src
              ./test
            ];
            compile = {
              compilerOptions = [ "--json-errors" ];
            };
          };

          psDemo = purs-nix-lib.purs {
            dependencies = baseDeps ++ [
              "console"
              "identity"
            ];
            srcs = [
              ./src
              ./demo
            ];
            compile = {
              compilerOptions = [ "--json-errors" ];
            };
          };

          # Source for linting - only PureScript files
          pursSource = pkgs.lib.fileset.toSource {
            root = ./.;
            fileset = pkgs.lib.fileset.unions [
              ./src
              (pkgs.lib.fileset.maybeMissing ./test)
              (pkgs.lib.fileset.maybeMissing ./whine.yaml)
            ];
          };

          # Nix lint source
          nixLintSource = pkgs.lib.fileset.toSource {
            root = ./.;
            fileset = pkgs.lib.fileset.fileFilter (file: file.hasExt "nix") ./.;
          };

          # Whine check - PureScript linting
          lint-whine = pkgs.stdenv.mkDerivation {
            pname = "purescript-flow-lint-whine";
            version = "1.0.0";

            src = pursSource;

            nativeBuildInputs = [
              inputs'.purescript-whine.packages.default
            ];

            buildPhase = ''
              runHook preBuild
              runHook postBuild
            '';

            checkPhase = ''
              runHook preCheck
              echo "Running PureScript whine check..."
              whine 'src/**/*.purs' || {
                echo "PureScript whine check failed!"
                exit 1
              }
              echo "PureScript whine check passed"
              runHook postCheck
            '';

            doCheck = true;

            installPhase = ''
              mkdir -p $out
              echo "Whine passed" > $out/result.txt
            '';
          };

          # Nix lint check - statix + deadnix
          lint-nix = pkgs.stdenv.mkDerivation {
            pname = "purescript-flow-lint-nix";
            version = "1.0.0";

            src = nixLintSource;

            nativeBuildInputs = [
              pkgs.statix
              pkgs.deadnix
              pkgs.fd
            ];

            buildPhase = ''
              runHook preBuild
              runHook postBuild
            '';

            checkPhase = ''
              runHook preCheck
              echo "Running Nix lint checks..."
              echo "  -> statix (anti-patterns)..."
              ${pkgs.fd}/bin/fd -e nix -x ${pkgs.statix}/bin/statix check {} \; || {
                echo "Nix statix check failed!"
                exit 1
              }

              echo "  -> deadnix (unused bindings)..."
              ${pkgs.fd}/bin/fd -e nix -x ${pkgs.deadnix}/bin/deadnix --fail {} \; || {
                echo "Nix deadnix check failed!"
                exit 1
              }
              echo "Nix lint passed"
              runHook postCheck
            '';

            doCheck = true;

            installPhase = ''
              mkdir -p $out
              echo "Nix lint passed" > $out/result.txt
            '';
          };

          # No FFI check - ensures no .js or .erl files in src/
          lint-no-ffi = pkgs.stdenv.mkDerivation {
            pname = "purescript-flow-lint-no-ffi";
            version = "1.0.0";

            src = pursSource;

            nativeBuildInputs = [
              pkgs.fd
            ];

            buildPhase = ''
              runHook preBuild
              runHook postBuild
            '';

            checkPhase = ''
              runHook preCheck
              echo "Checking for FFI files in src/..."

              JS_FILES=$(${pkgs.fd}/bin/fd -e js . src 2>/dev/null || true)
              ERL_FILES=$(${pkgs.fd}/bin/fd -e erl . src 2>/dev/null || true)

              if [ -n "$JS_FILES" ]; then
                echo ""
                echo "ERROR: Found .js files in src/:"
                echo "$JS_FILES"
                echo ""
                echo "purescript-flow is a pure PureScript library."
                echo "No FFI files (.js, .erl) are allowed in src/"
                exit 1
              fi

              if [ -n "$ERL_FILES" ]; then
                echo ""
                echo "ERROR: Found .erl files in src/:"
                echo "$ERL_FILES"
                echo ""
                echo "purescript-flow is a pure PureScript library."
                echo "No FFI files (.js, .erl) are allowed in src/"
                exit 1
              fi

              echo "No FFI files found - check passed"
              runHook postCheck
            '';

            doCheck = true;

            installPhase = ''
              mkdir -p $out
              echo "No FFI check passed" > $out/result.txt
            '';
          };

          # Aggregate lint check
          lint = pkgs.runCommand "purescript-flow-lint" { } ''
            mkdir -p $out
            cat ${lint-whine}/result.txt > $out/result.txt
            cat ${lint-nix}/result.txt >> $out/result.txt
            cat ${lint-no-ffi}/result.txt >> $out/result.txt
            echo "All lint checks passed" >> $out/result.txt
          '';

          # Library output - compiled PureScript
          lib = ps.output { };

          # Tests output - compiled PureScript including test modules
          testsCompiled = psWithTests.output { };

          # Demo bundle - bundled Node.js app
          demoBundle = psDemo.bundle {
            esbuild.outfile = "demo.js";
            module = "Demo.Main";
          };

          # Test bundle - bundled Node.js test runner
          testBundle = psWithTests.bundle {
            esbuild = {
              outfile = "test.js";
              platform = "node";
            };
            module = "Test.Main";
          };

          # purs-tidy wrapper for treefmt
          pursTidyWrapper = pkgs.writeShellScriptBin "purs-tidy-treefmt" ''
            exec ${pkgs.purs-tidy}/bin/purs-tidy format-in-place "$@"
          '';

        in
        {
          # Unified formatting
          treefmt = {
            projectRootFile = "flake.nix";
            programs = {
              nixfmt.enable = true;
              nixfmt.package = pkgs.nixfmt-rfc-style;
            };
            settings.formatter = {
              purs-tidy = {
                command = "${pursTidyWrapper}/bin/purs-tidy-treefmt";
                includes = [ "*.purs" ];
              };
            };
          };

          packages = {
            default = lib;
            inherit lib;
          };

          checks = {
            # Build check
            build = lib;

            # Tests compile check
            tests-compile = testsCompiled;

            # Tests run check
            test-playback =
              pkgs.runCommand "purescript-flow-test-playback"
                {
                  nativeBuildInputs = [ pkgs.nodejs ];
                }
                ''
                  ${pkgs.nodejs}/bin/node ${testBundle}
                  touch $out
                '';

            # Linting
            inherit
              lint-whine
              lint-nix
              lint-no-ffi
              lint
              ;

            # Format check
            format = config.treefmt.build.check config.treefmt.projectRoot;
          };

          apps = {
            # Format fix
            format-fix = {
              type = "app";
              program = toString (
                pkgs.writeShellScript "purescript-flow-format-fix" ''
                  #!/usr/bin/env bash
                  set -euo pipefail
                  echo "Formatting all files with treefmt..."
                  exec ${config.treefmt.build.wrapper}/bin/treefmt "$@"
                ''
              );
              meta.description = "Fix code formatting (Nix + PureScript)";
            };

            # Lint check
            lint = {
              type = "app";
              program = toString (
                pkgs.writeShellScript "purescript-flow-lint" ''
                  #!/usr/bin/env bash
                  set -euo pipefail

                  echo "Running all lint checks..."
                  echo ""

                  echo "-> whine (PureScript)..."
                  PATH="${pkgs.esbuild}/bin:${pkgs.purescript}/bin:$PATH" ${inputs'.purescript-whine.packages.default}/bin/whine 'src/**/*.purs' || {
                    echo "PureScript lint check failed!"
                    exit 1
                  }

                  echo "-> statix (Nix anti-patterns)..."
                  ${pkgs.fd}/bin/fd -e nix -x ${pkgs.statix}/bin/statix check {} \; || {
                    echo "Nix statix check failed!"
                    exit 1
                  }

                  echo "-> deadnix (Nix unused bindings)..."
                  ${pkgs.fd}/bin/fd -e nix -x ${pkgs.deadnix}/bin/deadnix --fail {} \; || {
                    echo "Nix deadnix check failed!"
                    exit 1
                  }

                  echo "-> no-ffi (checking for .js/.erl files)..."
                  JS_FILES=$(${pkgs.fd}/bin/fd -e js . src 2>/dev/null || true)
                  ERL_FILES=$(${pkgs.fd}/bin/fd -e erl . src 2>/dev/null || true)
                  if [ -n "$JS_FILES" ] || [ -n "$ERL_FILES" ]; then
                    echo "ERROR: Found FFI files in src/!"
                    [ -n "$JS_FILES" ] && echo "$JS_FILES"
                    [ -n "$ERL_FILES" ] && echo "$ERL_FILES"
                    exit 1
                  fi

                  echo ""
                  echo "All lint checks passed"
                ''
              );
              meta.description = "Run all lint checks";
            };

            # PureScript code quality tools
            dedup = {
              type = "app";
              program = "${inputs'.purescript-dedup.packages.default}/bin/dedup";
              meta.description = "Detect semantically similar PureScript functions";
            };

            scythe = {
              type = "app";
              program = "${inputs'.purescript-scythe.packages.default}/bin/scythe";
              meta.description = "Find unused PureScript functions";
            };

            whine = {
              type = "app";
              program = "${inputs'.purescript-whine.packages.default}/bin/whine";
              meta.description = "PureScript linter with custom rules";
            };

            drop = {
              type = "app";
              program = "${inputs'.purescript-drop.packages.default}/bin/drop";
              meta.description = "Find Pursuit library functions that could replace local code";
            };

            # Test runner
            test-playback = {
              type = "app";
              program = toString (
                pkgs.writeShellScript "purescript-flow-test-playback" ''
                  #!/usr/bin/env bash
                  set -euo pipefail
                  exec ${pkgs.nodejs}/bin/node ${testBundle}
                ''
              );
              meta.description = "Run playback tests";
            };

            # Demo application
            demo = {
              type = "app";
              program = toString (
                pkgs.writeShellScript "purescript-flow-demo" ''
                  #!/usr/bin/env bash
                  set -euo pipefail
                  exec ${pkgs.nodejs}/bin/node ${demoBundle}
                ''
              );
              meta.description = "Run the demo application";
            };
          };

          devShells.default = pkgs.mkShell {
            buildInputs = [
              pkgs.purs-tidy
              pkgs.nixfmt-rfc-style
              pkgs.statix
              pkgs.deadnix
              pkgs.fd

              # PureScript tooling via purs-nix
              (ps.command { })

              # CVT tools
              inputs'.purescript-dedup.packages.default
              inputs'.purescript-scythe.packages.default
              inputs'.purescript-drop.packages.default
              inputs'.purescript-whine.packages.default

              # CLAUDE.md generation
              inputs'.agen.packages.default
            ];

            shellHook = ''
              echo "======================================="
              echo "purescript-flow Development Environment"
              echo "======================================="
              echo ""
              echo "Primary Nix Commands:"
              echo "  nix flake check       - Run ALL checks (build, lint, format)"
              echo "  nix fmt               - Format all files (Nix + PureScript)"
              echo "  nix build             - Build library"
              echo ""
              echo "Apps:"
              echo "  nix run .#demo        - Run the demo application"
              echo "  nix run .#lint        - Run all lint checks"
              echo "  nix run .#format-fix  - Fix formatting"
              echo "  nix run .#dedup       - Find duplicate code"
              echo "  nix run .#scythe      - Find dead code"
              echo "  nix run .#drop        - Find library replacements"
              echo "  nix run .#whine       - Run PureScript linter"
              echo ""
              echo "PureScript:"
              echo "  purs-nix compile --json-errors - Compile PureScript"
              echo ""
              echo "======================================="

              # Regenerate CLAUDE.md
              agen >/dev/null 2>&1 || true
            '';
          };
        };
    };
}
