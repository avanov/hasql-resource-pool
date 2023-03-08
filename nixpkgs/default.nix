{}:

let

common-src = builtins.fetchTarball {
    name = "common-2023-01-03";
    url = https://github.com/avanov/nix-common/archive/26fc569bfd9871906cbbb57f2151c19753894620.tar.gz;
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "sha256:18n9k0qh1dhcp4p6vhlgwz4ps310k9yv5h6snnkjmwnn22wrpzmf";
};

pkgs     = (import common-src { projectOverlays = [ ]; }).pkgs;
ghcEnv   = import "${common-src}/ghc-env.nix";  # will have to be called with required arguments by the consumer

in

{
    inherit pkgs;
    inherit ghcEnv;
}
