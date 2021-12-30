let
	shrinker = import ./default.nix;
in
shrinker.checks.x86_64-linux."shrinker\:test\:shrinker-test"
