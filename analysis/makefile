EMAIL=youremail@yourserver.edu.au
SERVER=yourusername@yourserver.edu.au
mode=publication # e.g., check, publication
RSCRIPT=/usr/bin/Rscript

all:
	make models mode=$(mode)

models:
	$(RSCRIPT) -e "library(knitr); analysis_mode = '$(mode)'; knit('lavaan-models.rmd')"
	echo "Models on $(mode) is complete." | mail -s "Models on $(mode) is complete."  $(EMAIL) 

rmarkdown:	
	$(RSCRIPT) -e "Sys.setenv(RSTUDIO_PANDOC='/Applications/RStudio.app/Contents/MacOS/pandoc');library(rmarkdown);  library(utils); render('main-analysis.rmd', 'pdf_document')"
	$(RSCRIPT) -e "Sys.setenv(RSTUDIO_PANDOC='/Applications/RStudio.app/Contents/MacOS/pandoc');library(rmarkdown);  library(utils); render('analysis-lavaan.rmd', 'pdf_document')"

updatelocal:
	rsync -av --update $(SERVER):~/hexaco-applicants/ .
	
updateserver:
	rsync -av --update . $(SERVER):~/hexaco-applicants/ 
