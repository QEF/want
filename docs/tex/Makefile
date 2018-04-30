#
#===============================
# WanT package
#===============================
#


######################################

TEX=want_manual
#
FIG=`ls *.eps`
PDFFIG=`ls *.eps | sed 's/\.eps/\.pdf/'`
JPGFIG=`ls *.eps | sed 's/\.eps/\.jpg/'`
#
LATEX=latex
VIEWER=xdvi
#
PDFLATEX=pdflatex
PDFVIEWER=acroread
PDFCONVERTER=epstopdf
JPGCONVERTER=eps2png -jpg --resolution 200

.SUFFIXES : .eps .pdf .jpg
.eps.pdf  : 
	$(PDFCONVERTER) $<
.eps.jpg  : 
	$(JPGCONVERTER) $<

######################################


#
# manual
#
default:
	@echo
	@echo  "  *** Wannier-Transport (WanT) package *** "
	@echo  
	@echo  "  This Makefile provides a simple utility to "
	@echo  "  latex-compile the WanT manual. "
	@echo  "  Using pdflatex will make easier the production "
	@echo  "  of the pdf file. "
	@echo  "  NOTE: the only file to be compiled is want_manual.tex"
	@echo  
	@echo  "  Possible <target>'s are: "
	@echo  "     latex            compile the manual using latex"
	@echo  "     ps               produce a postscript file"
	@echo  "     pdf              produce a pdf file using pdflatex"
	@echo  "     update           update docs dir with the new manual"
	@echo  "     all              latex, pdf, update"
	@echo  
	@echo  "     view             open the dvi manual file"
	@echo  "     pdfview          open the pdf manual file"
	@echo  "     clean            back to the original status"
	@echo  


#
# Main target
#

all:    
	make latex
	make bib
	make latex
	make latex
	make latex
	make pdf
	make update

latex:    
	$(LATEX) $(TEX).tex

bib:
	bibtex $(TEX)

pdf:    pdffig
	$(PDFLATEX) $(TEX).tex

update:
	if test -e $(TEX).pdf ; then \
	cp $(TEX).pdf .. ; fi

pdffig:
	for fig in $(PDFFIG) ; do  \
	   ( make $$fig ) \
	done

jpgfig:
	for fig in $(JPGFIG) ; do  \
	   ( make $$fig ) \
	done

ps:
	dvips $(TEX).dvi

view:   
	$(VIEWER) $(TEX).dvi &

pdfview:   
	$(PDFVIEWER) $(TEX).pdf &


clean:
	-rm *.aux *.idx *.blg *.bbl *.dvi *.log *.toc \
	    $(PDFFIG) $(TEX).ps $(TEX).pdf

