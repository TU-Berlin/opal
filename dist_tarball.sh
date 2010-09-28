#!/bin/bash

cd ${0%/*}


. src/om/specs/ShSpecs.basic

echo $PATH

#./configure

tmpdir=`mktemp -d`

echo Using temporary directory $tmpdir

distdir=$tmpdir/ocs-$VERSION
tarball=ocs-$VERSION.tar.gz

mkdir $distdir

# Create C and intermediate files.
ocs

echo "Copying files to temporary directory..."
tar c --exclude=$tarball --exclude=.svn -f - . | tar x -C $distdir -f -

echo "Removing unnecessary files..."
find $distdir \( -name \*.o -o -name \*.a \) -exec rm {} \;

for f in `cat DELETE.exe`; do
    rm $distdir/$f
done
for f in `cat DELETE.ocsdefs`; do
    rm $distdir/$f
done
for f in `cat DELETE.OCS`; do
    rm -r $distdir/$f
done
for f in `cat DELETE.in`; do
    rm $distdir/$f
done

echo "Writing dummy intermediate files..."
for f in `find $distdir -name \*.inter -o -name \*.opt`; do
    mtime="`stat -c \"%y\" $f`"
    echo "Dummy intermediate file to enable bootstrapping. Do not change its mtime!" > $f
    touch -d "$mtime" $f
done

echo "Creating tarball $tarball..."
tar czf $tarball -C $tmpdir ocs-$VERSION

echo "Deleting temporary firectory..."
rm -rf $TMP
