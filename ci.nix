let
	shrinker = import ./default.nix;
in 
{
  tests = shrinker.checks.x86_64-linux."shrinker-testing\:test\:shriker-tests";
  build-tx =shrinker.packages.x86_64-linux."shrinker-tx\:lib\:shrinker-tx";
}

