
FIGURES = lines.pdf classes.pdf

%.pdf: %.eps
	epstopdf $<

history.pdf: history.tex $(FIGURES)
	pdflatex history

$(FIGURES:.pdf=.eps): history.dat plot-history.py
	python plot-history.py

clean:
	rm -f *.pdf *.eps *.aux *.log

