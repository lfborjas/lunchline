{system ? builtins.currentSystem}:
# NOTE(luis)
# All packages listed here will be built from source, as they're not
# 'blessed' in our pinned nix version. The sources themselves _are_
# obtained from cache.nixos.org, but need to be rebuilt regardless.
# Exercise restraint!
let
  dontCheck   = (import ./packages.nix{inherit system;}).haskell.lib.dontCheck;
  doJailbreak = (import ./packages.nix{inherit system;}).haskell.lib.doJailbreak;
in (super: {
  #servant = super.servant_0_18;
  #servant-server = super.servant-server_0_18;
  
  # NOTE: can't use this because cabal2nix depends on older versions of
  # directory/unix; adding them causes infinite recursion
  # time = super.time_1_12_1;
  # # NOTE: these two depend on time 1.9.3
  # directory = dontCheck super.directory_1_3_7_0;
  # unix = dontCheck super.unix_2_7_2_2;
})
