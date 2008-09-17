../../bin/c2newspeak: unknown option `-I'.
C2Newspeak version 1.3 build 2f2aa29dc3cd (2008-08-24).
Software under LGPL v. 2.1. Copyright EADS.
Authors Charles Hymans and Olivier Levillain.

Usage: ../../bin/c2newspeak [options] [-help|--help] [file...]

  --no-init disables zero initialisation of the globals
  --castor allows horrible casts to be translated
  --dirty allows dirty syntax
  --accept-forward-goto accepts forward goto statements
  --strict sets strict syntax
  --ignore-pragma ignores any #pragma directive
  --ignore-asm ignores any asm directive
  --ignore-pack ignores any packed attribute
  --ignore-volatile ignores 'volatile' type qualifier
  --keep-unused-vars does not remove unused variables
  --accept-extern does not raise an error on variables declared but not defined

  --accept-mult-def does not raise an error multiple definitions of the same variables

  --cil use CIL lexer and parser instead of our own
  --gnuc allow GNU C extensions
  --cil-printer verbose options: uses "default" or "plain" Cil output
  --more-warnings verbose options: displays more warnings
  --debug verbose options: displays more debugging info
  --ast verbose option: displays Abstract Syntax Tree output
  --npko verbose option: displays NewsPeak Object intermediate output
  --newspeak verbose option: displays Newspeak output
  --pretty verbose options: uses var names for Newspeak display
  -v verbose mode: turn all verbose options on
  -q quiet mode: turn display off
  -c compiles only into a .no file
  -o gives the name of Newspeak output

  --version prints the version of the software
  --no-opt Disables all code simplifications
  --one-loop Normalizes loops
  -help  Display this list of options
  --help  Display this list of options
