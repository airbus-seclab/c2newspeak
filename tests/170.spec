C2Newspeak version 1.0 build 20080103 by Olivier Levillain and Charles Hymans.
Software under LGPL v. 2.1. Copyright EADS.

Usage: ../bin/c2newspeak [options] [-help|--help] [file...]

  --no-init disables zero initialisation of the globals
  --castor allows horrible casts to be translated
  --ignore-pragma ignores any #pragma directive
  --keep-unused-vars does not remove unused variables
  --accept-extern does not raise an error on variables declared but not defined

  --accept-mult-def does not raise an error multiple definitions of the same variables

  --cil verbose option: displays CIL output
  --cil-printer verbose options: uses "default" or "plain" Cil output
  --more-warnings verbose options: displays more warnings
  --debug verbose options: displays more debugging info
  --npko verbose option: displays NewsPeak Object intermediate output
  --newspeak verbose option: displays Newspeak output
  --pretty verbose options: uses var names for Newspeak display
  -v verbose mode: turn all verbose options on
  -q quiet mode: turn display off
  --exit-code returns exit code 1 if an error occured

  -I includes a pre-processing directory
                     (must be repeated for each directory)

  -c compiles only into a .no file
  -o gives the name of Newspeak output

  --version prints the version of the software
  --no-opt Disables all code simplifications
  --one-loop Normalizes loops
  --experimental Use own lexer and parser instead of CIL: still experimental
  -help  Display this list of options
  --help  Display this list of options
