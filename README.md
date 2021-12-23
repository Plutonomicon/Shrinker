# Shrinker
This project contains tools for shrinking plutus scripts.

## Using Shrinker

### PlutusTX

For plutus TX Shrinker is still somewhat expirementall. You can use either `Shrink.PlutusTX.shrinkCompiled` or `Shrink.PlutusTXTH.shrinkCompiledTH`. Shrink can be fairly slow on large scripts, hopefully this will improve with further development, but with plutustx there also seem to be laziness issues where shrink will run on the same script many times causing it to be much slower.

### Other

In most other cases `Shrink.shrinkScript` is the only function you need. If you want to exclude particular tactics perhaps because they are slow or have a bug you can use `shrinkScriptSp (withoutTactics ["curry"]) scriptYouWantToShrink` . 

## Bugs

If you find a bug please report it on github or email me at `brian@mlabs.city`, a print of the uplc fed to shrink would be much appreciated. Feature requests and suggestions are also welcome. 
