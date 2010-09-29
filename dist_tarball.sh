#!/bin/bash

cd ${0%/*}


. src/om/specs/ShSpecs.basic

echo $PATH


if [ -x config.status ]; then
    ./config.status
else
    ./configure
fi

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
    rm -r $distdir/$f
done

# Delete TeX temporaries from doc.
texdocs="bibopalica dosfopman hcguide install oasysman opal2x opalreport tutorial userguide"
for d in $texdocs; do
    for suffix in `cat DELETE.tex`; do
	rm -f $distdir/doc/$d/*.$suffix
    done
done

# Delete DOSFOP temporaries.
for f in `cat DELETE.dosfop`; do
    rm -f $distdir/$f
done



# Delete emacs backup files.
find $distdir -name \*~ -exec rm {} \;


echo "Writing dummy intermediate files..."
for f in `find $distdir -name \*.inter -o -name \*.opt`; do
    mtime="`stat -c \"%y\" $f`"
    echo "Dummy intermediate file to enable bootstrapping. Do not change its mtime!" > $f
    touch -d "$mtime" $f
done

echo "Creating tarball $tarball..."
tar czf $tarball -C $tmpdir ocs-$VERSION

echo "Deleting temporary firectory..."
rm -rf $tmpdir
