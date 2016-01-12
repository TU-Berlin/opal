#!/bin/bash
#
# Call in root of ocs source tree.

if [ ! -f ocs-version ]; then
    error "Must be called in root of ocs source tree."
    exit 1
fi

echo "*** CLEANING SOURCE ***"
(cd src; ocs cleanall)
echo "*** BUILD LIBRARY ***"
(cd src/lib; ocs)
echo "*** BUILD GENMAKE (will not link) ***"
(cd src/om/genmake; ocs)
echo "*** BUILD COMPILER LIBRARY ***"
(cd src/oc/shared; ocs)
echo "*** BUILD OC2 (will not link) ***"
(cd src/oc/oc2; ocs)
echo "*** BUILD OC1 (will not link) ***"
(cd src/oc/oc1; ocs)

# Remove ocs from path
export PATH="/usr/lib/lightdm/lightdm:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games"

# Compile compiler and base library
echo "*** RECOMPILE AND INSTALL BASELIB AND COMPILER ***"
make lib.opal_base
make pkg.genmake
make lib.oc_oc1
make lib.oc_oc2
make pkg.oc1
make pkg.oc2

# Compile the rest
echo "*** RECOMPILE THE REST OF THE DISTRIBUTION ***"
make pkg.getopalname lib.opal_parserlight lib.opal_readline lib.opal_tcl \
    lib.opal_tk lib.opal_win pkg.browser pkg.emacs pkg.vim pkg.doc \
    pkg.ordinatrice lib.oasys_shared lib.oasys_main pkg.oasys pkg.evaluator
