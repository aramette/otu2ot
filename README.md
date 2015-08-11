otu2ot
======

A R package to determine oligotypes for DNA sequences.
The package provides R scripts to perform the oligotyping approach 
(http://oligotyping.org), as a way to exploring microbial patterns through 
subtle nucleotide variation within 16S rRNA gene sequences. 

To directly use the package into your R session, use any of the files:   
		* otu2ot_x.y.tar.gz (UNIX)   
		* otu2ot_x.y.zip (WINDOWS)


The R package is presented in: 
Ramette and Buttigieg, "The R package otu2ot for implementing the entropy decomposition of nucleotide 
variation in sequence data", Front. Microbiol., 14 November 2014 | doi: 10.3389/fmicb.2014.00601 
http://journal.frontiersin.org/article/10.3389/fmicb.2014.00601/full

The original oligotype paper is described in: 
Eren et al. 2013 Methods in Ecology and Evolution. 4,1111-1119. doi: 10.1111/2041-210X.12114.
http://onlinelibrary.wiley.com/doi/10.1111/2041-210X.12114/pdf

installation
=====
Assuming the file "otu2ot_1.4.tar.gz" is in the directory "C:/R/oligotyping/", type in your R console:
> setwd("C:/R/oligotyping/")

> install.packages("otu2ot_1.4.tar.gz", repos = NULL, type = "source")

> library(otu2ot)

> help(otu2ot)
