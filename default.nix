{   # To see the list of available compilers in the current nixpkgs snapshot, run 'make list-ghc-versions'
    haskellCompiler ? "ghc944"
    # this affects static linking as well as licensing mode,
    # GMP assumes GPL license fsor the entire project if linked statically
,   withGMP         ? false
}:

let
    commonEnv       = import ./nixpkgs {};
    pkgs            = commonEnv.pkgs;
    ghcEnv          = commonEnv.ghcEnv {
        pkgs             = pkgs;
        haskellCompiler  = haskellCompiler;
        isHaskellWithGMP = withGMP;
        haskellLibraries = hackagePkgs: with hackagePkgs; [
            haskell-language-server
            stylish-haskell
            cabal-install
            cabal2nix
        ];
    };

    macOsDeps = with pkgs; lib.optionals stdenv.isDarwin [
        darwin.apple_sdk.frameworks.CoreServices
        darwin.apple_sdk.frameworks.ApplicationServices
    ];

    devEnv  = pkgs.mkShellNoCC {
        # Sets the build inputs, i.e. what will be available in our
        # local environment.
        nativeBuildInputs = with pkgs; [
            cachix
            cacert
            glibcLocales
            gnumake
            gitAndTools.pre-commit
            postgresql
            ghcEnv.ghc
            zlib
        ] ++ macOsDeps;
        shellHook = ''
            export PROJECT_PLATFORM="${builtins.currentSystem}"
            export LANG=en_GB.UTF-8

            # https://cabal.readthedocs.io/en/3.4/installing-packages.html#environment-variables
            export CABAL_DIR=$PWD/.local/${builtins.currentSystem}/cabal

            # symbolic link to Language Server to satisfy VSCode Haskell plugins
            ln -s -f `which haskell-language-server` $PWD/hls.exe
        '';
    };

in

{
    inherit devEnv;
}
