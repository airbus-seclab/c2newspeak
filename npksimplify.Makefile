DIRS=newspeak npksimplify
NSFILES=normalize store copy_propagation inline var_hoist npksimplify
FILES=version newspeak/newspeak $(addprefix npksimplify/,$(NSFILES))
TARGET=npksimplify

include common.Makefile
