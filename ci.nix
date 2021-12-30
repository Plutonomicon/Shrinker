let
	shrinker = import ./default.nix;
in
shrinker.packages.x86_64-linux."shrinker-testing\:test\:shriker-tests"
shrinker-testing.checks.shriker-tests
