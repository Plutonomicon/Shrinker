let
	shrinker = import ./default.nix;
in 
{
	build-shrinker=shrinker.packages.x86_64-linux."shrinker\:lib\:shrinker";
  build-shrinker-tx =shrinker.packages.x86_64-linux."shrinker-tx\:lib\:shrinker-tx";
	property-tests = shrinker.checks.x86_64-linux."shrinker-testing\:test\:shrinker-testing";
	unit-test = shrinker.checks.x86_64-linux."shrinker-unit-testing\:test\:shriker-unit-testing";
}

