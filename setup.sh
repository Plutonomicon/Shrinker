#!/bin/bash

# Exit if any command throws an error
set -e

# Get the project and module names from command line args
# Probably want to make it so that we only need to provide one later
case $# in
  0)
    read -p "Enter the project name (e.g. gero-gov): " PROJECT_NAME
    read -p "Enter the module name (e.g. GeroGov): " MODULE_NAME
    NEW_GITHUB_LINK="https://github.com/mlabs-haskell/${PROJECT_NAME}"
    read -p "Enter the name of the github link (default: ${NEW_GITHUB_LINK}): " github_input
    if [[ $github_input != "" ]]
    then
      NEW_GITHUB_LINK=$github_input
    fi
  ;;

  2)
    PROJECT_NAME=$1
    MODULE_NAME=$2
    NEW_GITHUB_LINK="https://github.com/mlabs-haskell/${PROJECT_NAME}"
    read -p "Enter the name of the github link (default: ${NEW_GITHUB_LINK}): " github_input
    if [[ $github_input != "" ]]
    then
      NEW_GITHUB_LINK=$github_input
    fi
  ;;

  3)
    PROJECT_NAME=$1
    MODULE_NAME=$2
    NEW_GITHUB_LINK=$3
  ;;

  *)
    echo "You may run this script with 0, 2, or 3 arguments"
    echo "E.g.:"
    echo "    ${0}"
    echo "    ${0} gero-gov GeroGov"
    echo "    ${0} gero-gov GeroGov https://github.com/mlabs-haskell/gero-gov"
    exit 1
  ;;
esac

# Sed runs differently on Mac and Linux. Tell which one we're on
SED_COMMAND="sed -i "
if [[ $OSTYPE == 'darwin'* ]]
  then
    SED_COMMAND="sed -i'' "
fi

# Modify the cabal file
CABAL_FILE="${PROJECT_NAME}.cabal"
OLD_GITHUB_LINK="https://github.com/mlabs-haskell/CardStarter-LiquidityBridge"

mv liquidity-bridge.cabal $CABAL_FILE
$SED_COMMAND "s|${OLD_GITHUB_LINK}|${NEW_GITHUB_LINK}|g" $CABAL_FILE
$SED_COMMAND "s|liquidity-bridge|${PROJECT_NAME}|g" $CABAL_FILE
$SED_COMMAND "s|LiquidityBridge|${MODULE_NAME}|g" $CABAL_FILE

# Modify hie.yaml, ci.nix, haskell.nix, Makefile, and integrate.yaml
$SED_COMMAND "s|liquidity-bridge|${PROJECT_NAME}|g" hie.yaml
$SED_COMMAND "s|liquidity-bridge|${PROJECT_NAME}|g" nix/ci.nix
$SED_COMMAND "s|liquidity-bridge|${PROJECT_NAME}|g" nix/haskell.nix
$SED_COMMAND "s|liquidity-bridge|${PROJECT_NAME}|g" Makefile
$SED_COMMAND "s|liquidity-bridge|${PROJECT_NAME}|g" .github/workflows/integrate.yaml

# Modify the dummy source file and Spec file
$SED_COMMAND "s|LiquidityBridge|${MODULE_NAME}|" test/Spec.hs
$SED_COMMAND "s|LiquidityBridge|${MODULE_NAME}|" src/LiquidityBridge.hs
mv src/LiquidityBridge.hs src/${MODULE_NAME}.hs

# Create the cabal.project.local file with the sodium flag
echo "package cardano-crypto-praos" >> cabal.project.local
echo "  flags: -external-libsodium-vrf" >> cabal.project.local

# Make sure the permissions on format.sh are correct
chmod 755 .github/format.sh

# Commit the changes so gitclean doesn't delete renamed files
git add $CABAL_FILE hie.yaml nix/ Makefile .github/ src/ test/

# If the liquidity-bridge.cabal file was tracked, we need to tell git that it was renamed
git add liquidity-bridge.cabal 2>/dev/null || true

git commit -m "Initialise project name"

# Perform first build and test
nix-shell --run "cabal build && cabal test"

# Perform CI actions
nix-build nix/ci.nix

echo "Successfully renamed and built project. A commit containing the changes has already been added (but not pushed)."
echo "Happy coding!"
