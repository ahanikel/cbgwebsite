#!/bin/sh

find . -name '*.hs' -exec grep -l EitherT {} + |\
    while read f
    do
        ed $f <<'EOF'
1,$s/Control.Monad.Trans.Either/Control.Monad.Except      /g
1,$s/EitherT/ExceptT/g
w
EOF
    done

