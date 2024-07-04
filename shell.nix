{
  ghcVersion ? "9.8.2",
  nixpkgs ?
    builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/56fc115880db6498245adecda277ccdb33025bc2.tar.gz";
      sha256 = "0svpqk4zsx104n4m6zgizl4ybxy0j6kxfs1v32p2f1whp0ix6cfg";
    },
  system ? builtins.currentSystem,
}: let
  pkgs = import nixpkgs {inherit system;};

  inherit (pkgs) haskell;

  ghcVersionNix = builtins.replaceStrings ["."] [""] ghcVersion;

  hs = pkgs.haskell.packages."ghc${ghcVersionNix}".extend (final: prev: {
    # Disable tests to avoid dependency cycle.
    http-streams = haskell.lib.dontCheck prev.http-streams;
    # Disable broken tests (forkIO nrOfRunning fails).
    threads = haskell.lib.dontCheck prev.threads;
  });

  haskell-language-server-wrapper = pkgs.haskell-language-server.override {
    haskellPackages = hs;
    supportedGhcVersions = [ghcVersionNix];
  };

  haskell-language-server-bin = pkgs.writeShellScriptBin "haskell-language-server" ''
    exec ${haskell-language-server-wrapper}/bin/haskell-language-server-${ghcVersion} "$@"
  '';

  project = hs.extend (final: prev: {
    heist = final.callCabal2nix "heist" deps/heist {};
    io-streams = final.callCabal2nix "io-streams" deps/io-streams {};
    io-streams-haproxy = final.callCabal2nix "io-streams-haproxy" deps/io-streams-haproxy {};
    snap = final.callCabal2nix "snap" ./. {};
    snap-core = final.callCabal2nix "snap-core" deps/snap-core {};
    snap-server = final.callCabal2nix "snap-server" deps/snap-server {};
    xmlhtml = final.callCabal2nix "xmlhtml" deps/xmlhtml {};
  });
in
  project.shellFor {
    packages = ps:
      with ps; [
        heist
        io-streams
        io-streams-haproxy
        snap
        snap-core
        snap-server
        xmlhtml
      ];

    doBenchmark = true;

    nativeBuildInputs = with pkgs; [
      cabal-install
      haskell-language-server-bin
      haskell-language-server-wrapper
      pandoc
    ];
  }
