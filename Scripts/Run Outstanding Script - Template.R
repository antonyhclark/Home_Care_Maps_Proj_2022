## Run Home Care Dashboard


# Set working directory
setwd("/conf/LIST_analytics/Forth Valley/Falkirk/HomeCare/Mapping/LIST Home Care Tools")

# Set the date of the file sent through
run_date = "15 11 21"


# Run processing script
source('/conf/LIST_analytics/Forth Valley/Falkirk/HomeCare/Mapping/LIST Home Care Tools/Scripts/HC Outstanding Map Process - Template.R')

# Run the dashboard RMD file

rmarkdown::render("Scripts/HC Outstanding Map RMD Oct 21.Rmd",
                  output_file = paste0("Outputs/", run_date, " Outstanding POCs Map.html"))

