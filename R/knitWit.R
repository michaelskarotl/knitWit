#!/usr/bin/env Rscript

####################################
# Project Begin: February 27, 2024
# Project End: 
# Author: Michael Skaro
# Purpose: Use the skeleton script to create a helper script to add a table to the 
# rna_val_db in the /Users/michael.skaro/Research/tempusRepos/bioinf-rna-onco-verification/device_validation/rnaval_db/rnaval_data/rnaval.db
# directory. 
# Functions:
#   1. Load the database with RSQL-lite
#   2. filter the database for the database for the desired data product
#   3. create a table to add to the database
#   4. file io, removed Mar 5, 2024. 
# Usage: R script to be invoked and interacted with from the terminal.
# Parameters: 
# Rscript Rscript rna_val_db_to_rad-study-design_DPs.R -i /Users/michael.skaro/Research/tempusRepos/bioinf-rna-onco-verification/device_validation/rnaval_db/rnaval_data/db/rnaval.db -p 1.3 -o .  -x /Users/michael.skaro/Research/tempusRepos/bioinf-rna-onco-verification/helpers/db_to_rad/PCL-00089_ref.txt -t BFXA-4210
# ticket ID: "BFXA-4210_RNA-val_DB_to_rad-study-x_DPs"
# Note: Once the output csv has been accepted by the product team, the output will be to append the table onto the 
# Things to note: UHR = 6 well-known gene fusions used for our benchmarking study on short-read sequencing data include BCAS4-BCAS3, BCR-ABL1, ARFGEF2-SULF2, RPS6KB1-TMEM49(VMP1), TMPRSS2-ERG, and GAS6-RASA3.
# Outputs: output a flat file to the output directory, an renv.lock and a session info file to the output directory

readRenviron("~/.Renviron")
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", repos = "http://cran.us.r-project.org")
}

if (file.exists("renv.lock")) {
  library(renv)
  renv::restore()
  # Call the libraries
  library(renv)
  library(optparse)
  library(tidyverse)
  library(data.table)
  library(DBI)
  library(RSQLite)
  library(languageserver)
  # included in tidyverse install
  library(stringr)
  library(dplyr, warn.conflicts = FALSE)
  library(dbplyr)
  library(knitr)
  library(devtools)
  library(conflr)
}

if (!file.exists("renv.lock")) {
  renv::init()
}

# install jsonlite package if not already installed
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  install.packages("jsonlite", repos = "http://cran.us.r-project.org")
}



# Now that we have a consistent R environment, we can install the necessary packages
print("Installing the necessary packages with renv loaded, this takes a while the first time, go get some coffee []D")

if (!requireNamespace("optparse", quietly = TRUE)) {
  install.packages("optparse", repos = "http://cran.us.r-project.org")
}

# install the libraries from the cran repo and the bioconductor repo to conduct the differential expression analysis in a nexflow pipeline

# install the tidyverse package if not already installed
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
}

# install data.table package if not already installed
if (!requireNamespace("data.table", quietly = TRUE)) {
  install.packages("data.table", repos = "http://cran.us.r-project.org")
}

# install DBI package if not already installed
if (!requireNamespace("DBI", quietly = TRUE)) {
  install.packages("DBI", repos = "http://cran.us.r-project.org")
}

# install languageserver package if not already installed
if (!requireNamespace("languageserver", quietly = TRUE)) {
  install.packages("languageserver", repos = "http://cran.us.r-project.org")
}

# install devtools package if not already installed
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools", repos = "http://cran.us.r-project.org")
}

# install knitr package if not already installed, not sure if this is in tidyverse
if (!requireNamespace("knitr", quietly = TRUE)) {
  install.packages("knitr", repos = "http://cran.us.r-project.org")
}

# install RSQLite package if not already installed, not sure if this is in tidyverse
if (!requireNamespace("RSQLite", quietly = TRUE)) {
  install.packages("RSQLite", repos = "http://cran.us.r-project.org")
}


# Call the libraries
library(renv)
library(optparse)
library(tidyverse)
library(data.table)
library(DBI)
library(RSQLite)
library(languageserver)
library(stringr)
library(dplyr, warn.conflicts = FALSE)
library(dbplyr)
library(knitr)
library(devtools)
library(conflr)
library(magrittr)
library(ggplot2)
library(tidyverse)
library(binom)
library(glue)
library(dplyr)
library(jsonlite)



# Define the command line arguments

option_list <- list(
  make_option(c("-t", "--tsv"), type = "character", default = NULL,
              help = "The tsv file with the list of the PCLs to knitted. The csv file should have four columns. \n
              The first column (PCL_ID) should be the path to the PCL-ID that needs to be knitted \n
              The second column (Report_File) should be the name of the final report file that has the knitting instructions for the subsequent analyses inside the PCL \n
              The third column (Appendix_RMDs) should be a comma separated list of the appendix RMD files. We will check that the appendix RMD files are in the PCL directory before knitting the PCL \n
              The fourth column (Library_File) will be the path to the library file that will be sourced in the knitting process. We will need to make sure that the libraries are installed in the R environment before knitting the PCL. \n
              ")
)

# Parse the command line arguments
opt <- parse_args(OptionParser(option_list = option_list))

# load the csv file into a data frame
df <- data.table::fread(opt$csv, header = TRUE)

# check that the csv file has the correct columns

if (ncol(df) != 4) {
  stop("The csv file does not have the correct number of columns. Please check the csv file and try again.")
}

# check that the csv file has the correct column names

if (colnames(df) != c("PCL_ID", "Report_File", "Appendix_RMDs", "Library_File")) {
  stop("The csv file does not have the correct column names. Please check the csv file and try again.")
}

# if the Appendix_RMDs column is empty, then there are no appendix RMDs to knit.
# if the Appendix_RMDs column is not empty, then we need to check that the appendix RMDs are in the PCL directory before knitting the PCL

if (any(df$Appendix_RMDs != "")) {
  # check that the appendix RMDs are in the PCL directory
  for (i in 1:nrow(df)) {
    appendix_rmds <- unlist(strsplit(df$Appendix_RMDs[i], ","))
    for (j in 1:length(appendix_rmds)) {
      if (!file.exists(file.path(df$PCL_ID[i], appendix_rmds[j]))) {
        stop(glue("The appendix RMD file {appendix_rmds[j]} is not in the PCL directory {df$PCL_ID[i]}. Please check the PCL directory and try again."))
      }
    }
  }
}

# check that the library file is in the PCL directory

if (!file.exists(df$Library_File)) {
  stop(glue("The library file {df$Library_File} is not in the PCL directory. Please check the PCL directory and try again."))
}

# check that the libraries are installed in the R environment













