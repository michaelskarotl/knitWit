#!/usr/bin/env Rscript

####################################
# Project Begin: April 16, 2024
# Project End: lol
# Author: Michael Skaro
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

# This script will knit the PCLs in the PCL directory. The PCLs are the directories that contain the RMD files for the analyses. 
# The script will check that the appendix RMD files are in the PCL directory before knitting the PCL. The script will also check 
# that the libraries are installed in the R environment before knitting the PCL.

# if data table is not installed, install it

if (!requireNamespace("data.table", quietly = TRUE)) {
  install.packages("data.table", repos = "http://cran.us.r-project.org")
}
# install jsonlite package if not already installed
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  install.packages("jsonlite", repos = "http://cran.us.r-project.org")
}

# install the tidyverse package if not already installed
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
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













