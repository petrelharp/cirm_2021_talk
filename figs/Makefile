.PHONY : all

all : gilia_circles.png \
   	gilia_high_sigma.png \
   	gilia_density_results.png \
   	gilia_low_sigma.png \
	pme_dists.png \
	bigsmall_dvar.png \
	spatial_pedigree.png

include ../rules.mk

pme_dists.png bigsmall_dvar.png : make_figs.R
	Rscript $<
