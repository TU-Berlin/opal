#!/bin/bash
#
# dist_tarball.sh
#
# Handcrafted script to create a bootstrapping enabled distribution
# tarball. It is not elegant at all but works. It writes dummy
# intermediate files to cheat the ocs build system and compile the C
# files directly without looking at the Opal sources (for the base
# library and the compiler, the rest is compiled by the newly build
# Opal compiler).

delete_from () {
    file=$1
    for f in `cat $file`; do
	rm -rf $distdir/$f
    done
}

owd=`pwd`

echo cd ${0%/*}/..


. src/om/specs/ShSpecs.basic


if [ -x config.status ]; then
    ./config.status
else
    ./configure --enable-dosfop
fi

tmpdir=`mktemp -d`

echo Using temporary directory $tmpdir

distdir=$tmpdir/ocs-$VERSION
tarball=ocs-$VERSION.tar.gz

mkdir $distdir

# Create C and intermediate files.
ocs

# Use tar to copy files (excluding subversion files) to keep file
# timestamps untouched.
echo "Copying files to temporary directory..."
tar c --exclude=$tarball --exclude=.svn -f - . | tar x -C $distdir -f -

# Delete object and archive files.
echo "Remove objects and archives..."
find $distdir \( -name \*.o -o -name \*.a \) -exec rm {} \;

# Delete executables.
echo "Remove executables..."
delete_from release/exclude.executables

# Delete OcsDefs-SysDefs.
echo "Remove unnecessary OcsDefs-SysDefs files..."
delete_from release/exclude.ocsdefs-sysdefs

# Delete OCS directories.
echo "Remove unnecessary OCS direcories..."
delete_from release/exclude.OCS

# Delete autoconf generated files.
echo "Remove autoconf generated files..."
delete_from release/exclude.autogen

# Delete DOSFOP intermediates.
echo "Remove DOSFOP intermediates..."
delete_from release/exclude.dosfop
delete_from release/exclude.texi

# Delete intermediate files in the doc tree.
echo "Remove intermediate files in the documentation..."
texdocs="bibopalica dosfopman hcguide install oasysman opal2x opalreport tutorial userguide"
for d in $texdocs; do
    for suffix in `cat release/exclude.doc`; do
	rm -f $distdir/doc/$d/*.$suffix
    done
done

# Delete emacs backup files.
echo "Remove emacs backup files..."
find $distdir -name \*~ -exec rm {} \;

# Write dummy intermediate files for bootstrapping.
echo "Writing dummy intermediate files..."
for f in `find $distdir -name \*.inter -o -name \*.opt`; do
    mtime="`stat -c \"%y\" $f`"
    echo "Dummy intermediate file to enable bootstrapping. Do not change its mtime!" > $f
    touch -d "$mtime" $f
done

# Create tarball.
echo "Creating tarball $tarball..."
tar czf $tarball -C $tmpdir ocs-$VERSION

# Delete temporary directory.
echo "Deleting temporary firectory..."
rm -rf $tmpdir

cd $owd
