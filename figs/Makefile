.PHONY : all

all : gilia_circles.png \
   	gilia_high_sigma.png \
   	gilia_density_results.png \
   	gilia_low_sigma.png \
	pme_dists.png \
	bigsmall_dvar.png \
	spatial_pedigree.png \
	coauthors.png

include ../rules.mk

pme_dists.png bigsmall_dvar.png : make_figs.R
	Rscript $<

coauthors.png : alison.jpg Prof.Kurtz.jpg ian_letter.png Terence_Tsui.jpg
	montage $^ -geometry 200x200+2+2 $@
