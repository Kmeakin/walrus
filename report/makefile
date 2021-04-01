all: report.Rmd
	Rscript -e 'rmarkdown::render("grammar.Rmd")'
	Rscript -e 'rmarkdown::render("report.Rmd")'	

clean:
	rm -rf out
