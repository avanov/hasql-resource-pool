{   supportedGhcVersions ?  [ "966" ]
}:

let

common-src = builtins.fetchTarball {
    name = "common-2025-02-04";
    url = https://github.com/avanov/nix-common/archive/c1b4f92477c37ff7a387cc7c81061dd9bd510014.tar.gz;
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "sha256:12rqz684949638zqbksl2z73lcy6khzxx46p095wpz6jh503kkgg";
};

overlays    = import ./overlays.nix {};
nixpkgsDist = (import common-src { projectOverlays = [ overlays.globalPackageOverlay ]; inherit supportedGhcVersions; });
pkgs        = nixpkgsDist.pkgs;
ghcEnv      = import "${common-src}/ghc-env.nix";  # will have to be called with required arguments by the consumer

in

{
    inherit nixpkgsDist;  # for repl testing
    inherit pkgs;
    inherit ghcEnv;
}
