all: paper.pdf

clean:
	rm -f paper.pdf ghost paper.o *.log paper.hi *.aux ghostout.tex *.bbl *.blg

ghost: paper.lhs
	ghc --make paper.lhs -o $@

ghostout.tex: ghost
	./ghost > $@

#paper.pdf: paper.lhs ghostout.tex
paper.pdf: paper.lhs bibliography.bib tree.tex
	pdflatex paper.lhs 
	bibtex paper 
	pdflatex paper.lhs 
	pdflatex paper.lhs 
