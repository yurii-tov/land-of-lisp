sbcl --non-interactive --eval '(progn (load "src/orc-battle.cl") (save-lisp-and-die "monster-battle.exe" :executable t :toplevel #'"'orc-battle-cli))"
