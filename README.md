# knitwit R package

![knitwit](images/knitwit.jpeg)

## Description

The knit wit R package will be a collection of functions that will help users to create and manipulate knitting patterns. The package will include functions to create a knitting pattern, to visualize the pattern, and to manipulate the pattern to HTML, PDF, or confluence format.

## Installation

You can install the development branch from our GitHub repository using the following code:

```r
devtools::install_github("michaelskarotl/knitwit")

```

## Usage

This package is currently under development. The following functions are available:

- `create_pattern()`: Create a knitting pattern
- `visualize_pattern()`: Visualize the knitting pattern
- `to_html()`: Convert the knitting pattern to HTML
- `to_pdf()`: Convert the knitting pattern to PDF
- `to_confluence()`: Convert the knitting pattern to Confluence format

## Example for creating a knitting pattern in confluence format

```r

if (!requireNamespace("knitwit", quietly = TRUE)) {
  devtools::install_github("michaelskarotl/knitwit")
}


library(knitwit)

# Create a knitting pattern
pattern <- create_pattern(
  title: "`r params$study_id` Nucleic Acid Extraction QC Report"
author: Michael Skaro
date: "`r format(Sys.time(), '%d %B, %Y')`"
params:
  study_id: PCL-00230
  Pipeline_version: 1.3.0
output: 
  conflr::confluence_document:
    space_key: "XXXXXXXX"
    parent_id: 'XXXXXXXX'
    type: "page"
    toc_depth: 7
    code_folding: "none"
    toc: TRUE
    update: TRUE
    use_original_size: TRUE
    interactive: FALSE
  )

# pass a master library file to the to_confluence function
ml <- data.table::fread("master_library_set.csv")

# the master library file is a data.table object that will ensure your libraries are installed and loaded
 # pass the pattern to the to_confluence function

knitwit::to_confluence(pattern = pattern, master_library = ml)

```



