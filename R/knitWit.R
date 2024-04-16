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

if (!requireNamespace("glue", quietly = TRUE)) {
  install.packages("glue", repos = "http://cran.us.r-project.org")
}

# install the rmarkdown package if not already installed
if (!requireNamespace("rmarkdown", quietly = TRUE)) {
  install.packages("rmarkdown", repos = "http://cran.us.r-project.org")
}

# if dplyr is not installed, install it
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr", repos = "http://cran.us.r-project.org")
}

# install the stringr package if not already installed

if (!requireNamespace("stringr", quietly = TRUE)) {
  install.packages("stringr", repos = "http://cran.us.r-project.org")
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

# call the libraries

library(data.table)
library(jsonlite)
library(tidyverse)
library(glue)
library(rmarkdown)
library(dplyr)
library(stringr)
library(DBI)
library(languageserver)
library(devtools)
library(knitr)
library(RSQLite)


make_obj <- function(file){
  # :param file: the csv file that contains the libraries that are required
  # :return: a data of the libraries that are required

  # load the csv file into a data frame with the data.table package
  df <- data.table::fread(file, header = TRUE)
  return df
}

# make a function to iterate through the make_libs_df and install each of the libraries
# the first column should be the library name and the second column should be the version of the library
# finally the third column should be the repository where the library is located. For example we will
# use the dpyr package as an example. The first column will be dplyr, the second column will be 1.0.0
# and the third column will be http://cran.us.r-project.org

install_libs <- function(df){
  # :param df: a data frame that contains the libraries that are required
  # :return: a list of the libraries that were installed

  counter = 0
  # iterate through the data frame and install each of the libraries
  for (i in 1:nrow(df)) {
    # install the library
    # if the second column is CRAN, then install the library from CRAN

    if (df$Repository[i] == "CRAN") {
      install.packages(df$Library[i], repos = "http://cran.us.r-project.org")
    }

    # if the second column is GitHub, then install the library install devtools and install the library from GitHub

    if (df$Repository[i] == "GitHub") {
      install.packages("devtools", repos = "http://cran.us.r-project.org")
      devtools::install_github(df$Library[i])
    }

    # if the second column is Bioconductor, then install the biocmanager package and install the library from Bioconductor

    if (df$Repository[i] == "Bioconductor") {
      if (!requireNamespace("BiocManager", quietly = TRUE)) {
        install.packages("BiocManager", repos = "http://cran.us.r-project.org")
      }
      BiocManager::install(df$Library[i])
    }

    # if the second column is none of the above, flag an error

    if (df$Repository[i] != "CRAN" & df$Repository[i] != "GitHub" & df$Repository[i] != "Bioconductor") {
      stop("The repository is not valid. Please check the repository and try again.")
    }
    
    if(counter == 0){
      # create a list of the libraries that were installed
      libs_installed <- list(df$Library[i])
      counter = counter + 1
    } else {
      libs_installed <- c(libs_installed, df$Library[i])
    }
  }
  return libs_installed
}

# make a function to take the list of the libraries that were installed and check that the libraries are installed

check_libs <- function(libs_installed){
  # :param libs_installed: a list of the libraries that were installed
  # :return: a list of the libraries that were not installed

  # create a list of the libraries that were not installed
  libs_not_installed <- list()

  # iterate through the list of the libraries that were installed and check that the libraries are installed
  for (i in 1:length(libs_installed)) {
    if (!requireNamespace(libs_installed[i], quietly = TRUE)) {
      libs_not_installed <- c(libs_not_installed, libs_installed[i])
    }
  }

  # check the length of the list of the libraries that were not installed
  # is greater than 0, then return the list of the libraries that were not installed

  if (length(libs_not_installed) > 0) {
    print("The following libraries were not installed:")
    for (i in 1:length(libs_not_installed)) {
      print(libs_not_installed[i])
      print(" ")
      print("Please install the library outside of the library and try again.")
    }

  } else {
    print("All of the libraries were installed successfully.")
  }

}


# make function to knit files based on a datatable

knit_files_to_HTML <- function(df){
  # :param df: a data table that contains the PCL_ID, Report_File, Appendix_RMDs, and Library_File
  # :return: a list of the files that were knitted

  # create a list of the files that were knitted
  files_knitted <- list()

  # iterate through the data table and knit the files
  for (i in 1:nrow(df)) {
    # knit the report file
    rmarkdown::render(input = df$Report_File[i], output_format = "html_document", output_file = df$PCL_ID[i])

    # knit the appendix RMD files
    appendix_rmds <- unlist(strsplit(df$Appendix_RMDs[i], ","))
    for (j in 1:length(appendix_rmds)) {
      rmarkdown::render(input = file.path(df$PCL_ID[i], appendix_rmds[j]), output_format = "html_document", output_file = file.path(df$PCL_ID[i], str_replace(appendix_rmds[j], ".Rmd", ".html")))
    }

    # knit the library file
    rmarkdown::render(input = df$Library_File[i], output_format = "html_document", output_file = file.path(df$PCL_ID[i], "library.html"))

    # add the files that were knitted to the list of the files that were knitted
    files_knitted <- c(files_knitted, df$PCL_ID[i])
  }

  return files_knitted
}

# make a function to check that the files were knitted into HTML files

check_files_knitted <- function(files_knitted){
  # :param files_knitted: a list of the files that were knitted
  # :return: a list of the files that were not knitted

  # create a list of the files that were not knitted
  files_not_knitted <- list()

  # iterate through the list of the files that were knitted and check that the files were knitted
  for (i in 1:length(files_knitted)) {
    if (!file.exists(file.path(files_knitted[i], "index.html"))) {
      files_not_knitted <- c(files_not_knitted, files_knitted[i])
    }
  }

  # check the length of the list of the files that were not knitted
  # is greater than 0, then return the list of the files that were not knitted

  if (length(files_not_knitted) > 0) {
    print("The following files were not knitted:")
    for (i in 1:length(files_not_knitted)) {
      print(files_not_knitted[i])
      print(" ")
      print("Please check the files and try again.")
    }

  } else {
    print("All of the files were knitted successfully.")
  }

}










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













