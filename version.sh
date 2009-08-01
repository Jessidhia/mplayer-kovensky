#!/bin/sh

test "$1" && extra="-$1"

git_revision=`git rev-list HEAD -n 1 | head -c 7`
test $git_revision || git_revision=UNKNOWN

NEW_REVISION="#define VERSION \"GIT-${git_revision}${extra}-Kovensky-mt `date -u +%Y%m%d`\""
OLD_REVISION=$(head -n 1 version.h 2> /dev/null)

TITLE='#define MP_TITLE "%s "VERSION" (C) 2000-2009 MPlayer Team\n"'

# Update version.h only on revision changes to avoid spurious rebuilds
if test "$NEW_REVISION" != "$OLD_REVISION"; then
    cat <<EOF > version.h
$NEW_REVISION
$TITLE
EOF
fi

