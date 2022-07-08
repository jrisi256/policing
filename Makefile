################################################ Cleaning and Processing Data

################ Path for R to find Pandoc
PANDOC = /usr/lib/rstudio/bin/quarto/bin

################# Paths to all directories in cleaning and processing
CLEAN = clean-and-process/
CLEAN_IN = $(CLEAN)input/
CLEAN_REPORTS = $(CLEAN)reports/
CLEAN_OUT = $(CLEAN)output/
CLEAN_SRC = $(CLEAN)src/
CLEAN_INPUT_VARS = $(CLEAN_IN)*.csv

################# Rmd file names and PDFs
DATA_SCHEMA = data-schemas
OFFICER_SHIFTS = merge_officers_assignments
IMPUTE = imputation-officer-assignments
BA_OUTCOMES = ba-merge_outcomes_officer-assignments
RISI_OUTCOMES = risi-merge_outcomes_officer-assignments

################## Initially clean data and generate the schemas for the data
$(CLEAN_OUT)cleaned-%.csv: $(CLEAN_SRC)$(DATA_SCHEMA).Rmd $(CLEAN_INPUT_VARS)
	Rscript -e "a <- commandArgs(trailingOnly = T); Sys.setenv(RSTUDIO_PANDOC = a[1]); rmarkdown::render(a[2], output_dir = a[3])" $(PANDOC) $< $(CLEAN_REPORTS)

$(CLEAN_REPORTS)$(DATA_SCHEMA).pdf: $(CLEAN_SRC)$(DATA_SCHEMA).Rmd $(CLEAN_INPUT_VARS)
	Rscript -e "a <- commandArgs(trailingOnly = T); Sys.setenv(RSTUDIO_PANDOC = a[1]); rmarkdown::render(a[2], output_dir = a[3])" $(PANDOC) $< $(CLEAN_REPORTS)

################### Merge the officer roster to shift assignments
$(CLEAN_OUT)officers_assignments_%.csv: $(CLEAN_SRC)$(OFFICER_SHIFTS).Rmd $(CLEAN_OUT)cleaned-officers.csv $(CLEAN_OUT)cleaned-assignments.csv
	Rscript -e "a <- commandArgs(trailingOnly = T); Sys.setenv(RSTUDIO_PANDOC = a[1]); rmarkdown::render(a[2], output_dir = a[3])" $(PANDOC) $< $(CLEAN_REPORTS)

$(CLEAN_REPORTS)$(OFFICER_SHIFTS).pdf: $(CLEAN_SRC)$(OFFICER_SHIFTS).Rmd $(CLEAN_OUT)cleaned-officers.csv $(CLEAN_OUT)cleaned-assignments.csv
	Rscript -e "a <- commandArgs(trailingOnly = T); Sys.setenv(RSTUDIO_PANDOC = a[1]); rmarkdown::render(a[2], output_dir = a[3])" $(PANDOC) $< $(CLEAN_REPORTS)

################### Impute missing shift times
$(CLEAN_REPORTS)$(IMPUTE).pdf: $(CLEAN_SRC)$(IMPUTE).Rmd $(CLEAN_OUT)officers_assignments_risi.csv
	Rscript -e "a <- commandArgs(trailingOnly = T); Sys.setenv(RSTUDIO_PANDOC = a[1]); rmarkdown::render(a[2], output_dir = a[3])" $(PANDOC) $< $(CLEAN_REPORTS)

################### Make outcomes, both the risi and ba versions
$(CLEAN_REPORTS)$(BA_OUTCOMES).pdf: $(CLEAN_SRC)$(BA_OUTCOMES).Rmd $(CLEAN_OUT)cleaned-*.csv $(CLEAN_OUT)officers_assignments_ba.csv
	Rscript -e "a <- commandArgs(trailingOnly = T); Sys.setenv(RSTUDIO_PANDOC = a[1]); rmarkdown::render(a[2], output_dir = a[3])" $(PANDOC) $< $(CLEAN_REPORTS)

$(CLEAN_REPORTS)$(RISI_OUTCOMES).pdf: $(CLEAN_SRC)$(RISI_OUTCOMES).Rmd $(CLEAN_OUT)cleaned-*.csv $(CLEAN_OUT)officers_assignments_risi.csv
	Rscript -e "a <- commandArgs(trailingOnly = T); Sys.setenv(RSTUDIO_PANDOC = a[1]); rmarkdown::render(a[2], output_dir = a[3])" $(PANDOC) $< $(CLEAN_REPORTS)

###################
.PHONY : clean_data make_officer_assignments impute ba_outcomes risi_outcomes
clean_data : $(CLEAN_REPORTS)$(DATA_SCHEMA).pdf
merge_officer_assignments: $(CLEAN_REPORTS)$(OFFICER_SHIFTS).pdf
impute: $(CLEAN_REPORTS)$(IMPUTE).pdf
ba_outcomes: $(CLEAN_REPORTS)$(BA_OUTCOMES).pdf
risi_outcomes: $(CLEAN_REPORTS)$(RISI_OUTCOMES).pdf
all: ba_outcomes risi_outcomes