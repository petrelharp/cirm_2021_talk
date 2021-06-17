SHELL := /bin/bash
# use bash for <( ) syntax

.PHONY : setup

cirm-2021.slides.html : setup

setup :
	$(MAKE) -C figs

include rules.mk
