Excerpt from `LICENSE`:
```
Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
```
# This project is Currently unmaintained

[#Shrinker](#Shrinker)
This project contains tools for shrinking plutus scripts.

## Using Shrinker

### PlutusTX

For plutus TX Shrinker is still somewhat experimental. You can use either `Shrink.PlutusTX.shrinkCompiled` or `Shrink.PlutusTXTH.shrinkCompiledTH`, both of which require you build `shrinker-tx` from the `./shrinker-tx` directory. Shrink can be fairly slow on large scripts, hopefully this will improve with further development, but with plutustx there also seem to be laziness issues where shrink will run on the same script many times causing it to be much slower.

### Other

In most other cases `Shrink.shrinkScript` is the only function you need. If you want to exclude particular tactics perhaps because they are slow or have a bug you can use `shrinkScriptSp (withoutTactics ["curry"]) scriptYouWantToShrink` .

## Bugs

If you find a bug please report it on github or email me at `brian@mlabs.city`, a print of the uplc fed to shrink would be much appreciated. Feature requests and suggestions are also welcome.

## How it works

Shrinker uses a number of tactics which make small changes to code which may reduce the size and runs a search keeping track of the smallest scripts produced by these tactics until nothing smaller is found.

### Tactics

#### Subs

Subs applies a beta-reduction, this often corresponds to inlining a function and can reduce script size if the function is only used once.

#### Curry

Curry currys a function.

#### Weak Unsubs

Weak unsubs finds an expression which appears at least twice and undoes a beta-reduction. This often corresponds to common sub expression elimination.

#### Strong Unsubs

Strong unsubs is a more general version of weak unsubs, it also corresponds to undoing beta-reductions but it invents a lambda function in order to make the reductions possible.

ie. `((2 + 1) + (3 + 1))` might reduce to `(\f -> f 2 + f 3)(\x -> x + 1)`

#### Remove Dead Code

Remove dead code looks for beta reductions where the variable bound by the lambda is not referenced, or where the argument is another variable. In either case the reduction always makes the code smaller so it's separated from Subs for performance.

#### Clean Pairs

Clean Pairs is a simple tactic required to make currying useful.

It reduces `fst (x,_)` to `x` and `fst (_,y)` to `y`. It's required to make currying useful.

#### Clean Force Delay

Clean Force Delay is a simple tactic that removes delays which are imediately forced.
ie `!#()` becomes `()` to use pluto syntax.

#### Promote Errors

Promote Errors looks for terms which will always error and replaces them with Error.
ie `1 + Error` becomes `Error`
