PUBSITE = kinglab.eeb.lsa.umich.edu:/var/www/html/ICTP_Spring_College

REXE = R --vanilla
RSCRIPT = Rscript --vanilla
RCMD = $(REXE) CMD
PDFLATEX = pdflatex
BIBTEX = bibtex
MAKEIDX = makeindex
CP = cp
RM = rm -f

default: index.html

publish: index.html
	rsync -avz --delete-after --exclude=cache --exclude=figure --chmod=a+rX,go-w $+ $(PUBSITE)

%.html: %.Rmd
	PATH=/usr/lib/rstudio/bin/pandoc:$$PATH \
	Rscript --vanilla -e "rmarkdown::render(\"$*.Rmd\",output_format=\"html_document\")"

%.html: %.md
	PATH=/usr/lib/rstudio/bin/pandoc:$$PATH \
	Rscript --vanilla -e "rmarkdown::render(\"$*.md\",output_format=\"html_document\")"

%.R: %.Rmd
	Rscript --vanilla -e "library(knitr); purl(\"$*.Rmd\",output=\"$*.R\")"

clean:
	$(RM) *.o *.so *.log *.aux *.out *.nav *.snm *.toc *.bak
	$(RM) Rplots.ps Rplots.pdf

fresh: clean
	$(RM) cache figure
