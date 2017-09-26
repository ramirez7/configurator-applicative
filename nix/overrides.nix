let
  pkgs = import ../nix/pkgs.nix;
in {
  s-cargot = pkgs.fetchFromGitHub {
    owner = "aisamanra";
    repo = "s-cargot";
    rev = "872b4673a64fee9c3a21a076a1052ae77f2c257e";
    sha256 = "0qhayjaaa4f4rp614wcp4y27g5ri7cci2is568mcwpgr73p0rs6y";
  };

  configurator = pkgs.fetchFromGitHub {
    owner = "jhickner";
    repo = "configurator";
    sha256 = "0a2x64m73m2x3w4fdvfvqyszsdibqikv00wx074im49hfms8n5cg";
    rev = "35369c98550d8b8003049697c6c9106f96d23103";
  };
}
