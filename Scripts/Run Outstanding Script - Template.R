# ******************************************************************************
# Author: Tony Clark, antony.clark2@phs.scot
# Tue Mar 22 17:16:17 2022
# Script purpose: this script runs an .R script that pre-processes
# the raw data then renders an .Rmd that uses objects created by the .R
# script
# ******************************************************************************

# Run Home Care Dashboard

# Set the date of the file sent through
# This date format is assumed to lead the file name
# e.g. for rundate "15 11 21", the raw data file is assumed to exist as
# Data_dummy/15 11 21 Weekly Provider Order SL dummy.xlsx
# or Data/15 11 21 Weekly Provider Order.xlsx
run_date = "15 11 21"

# Run processing script - objects created by this are used by the Rmd
source('Scripts/HC Outstanding Map Process - Template.R',verbose = F)

# Run the dashboard RMD file
html_file_path <- paste0("Outputs_dummy/", 
                         run_date, " ", 
                         "Outstanding POCs Map.html")

rmarkdown::render("Scripts/HC Outstanding Map RMD Template.Rmd",
                  output_file = html_file_path)

# Create a zip archive of the html output at the same time
zip::zip(gsub("\\.html$","\\.zip",html_file_path),
         html_file_path)

