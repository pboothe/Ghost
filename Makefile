all: paper.pdf ghost

clean:
	rm -f paper.pdf ghost paper.o paper.log paper.hi paper.aux ghostout.tex

ghost: paper.lhs
	ghc --make paper.lhs -o $@

ghostout.tex: ghost
	./ghost > $@

#paper.pdf: paper.lhs ghostout.tex
paper.pdf: paper.lhs bibliography.bib
	pdflatex paper.lhs 
	bibtex paper 
	pdflatex paper.lhs 
	pdflatex paper.lhs 
