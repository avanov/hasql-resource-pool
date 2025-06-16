{   supportedGhcVersions ?  [ "9102" ]
}:

let

common-src = builtins.fetchTarball {
    name = "common-2025-06-15";
    url = https://github.com/avanov/nix-common/archive/5a6693b065ceac109576a05df991190f842b33c0.tar.gz;
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "sha256:1k2bbs9mhybssxx5m57c48p2yd3xsxii7n3fmkgpgjmhin8praa9";
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
