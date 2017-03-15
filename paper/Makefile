all: CaseStudy1.html

clean: 
	rm -f CaseStudy1.md CaseStudy1.html

CaseStudy1.html: CaseStudy1.Rmd
	Rscript -e 'rmarkdown::render("$<")'