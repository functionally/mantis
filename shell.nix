let
  project = import ./default.nix;
in
  project.shellFor {

    packages = ps: with ps; [
      mantra
    ];

    # Set the following to `false` do disable the lengthy building of documentation.
    withHoogle = true;

     # See overlays/tools.nix for more details
    tools = {
      cabal                   = "latest";
      ghcide                  = "latest";
      haskell-language-server = "latest";
    # hdevtools               = "latest";
    # hindent                 = "latest";
      hlint                   = "latest";
      pointfree               = "latest";
    # pointfull               = "latest";
    };

    buildInputs = [ (import <nixpkgs> {}).git ];

    exactDeps = true;
  }
