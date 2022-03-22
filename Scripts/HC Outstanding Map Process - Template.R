# ******************************************************************************

### Home Care Requests - Mapping ##
## Ruben Vine ##
## Last Modified: Sept 2021 ##

##### Set up ######

# * Libraries ####
suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(leaflet)
  library(leaflet.extras)
  library(readr)
  library(readxl)
  library(tidyr)
  library(janitor)
  library(naniar)
  library(stringr)
  library(flexdashboard)
  library(rgdal)
  library(flextable)
  library(forcats)
  library(DT)
  library(crosstalk)  
})

# Parameters ####
hscp_of_interest <- "South Lanarkshire"
locality_vector <- c('Clydesdale','Hamilton',
                     'East Kilbride','Rutherglen Cambuslang')


# ******************************************************************************

# Run date
# This is set in the running script, but you can use it here for testing

# Set wd here if not using the other script
wd <- paste0(
  "/conf/LIST_analytics/Lanarkshire/Projects/Social Care/",
  "Home_Care_Maps_Proj_2022/"
)
setwd(wd)

run_date = "15 11 21"

# Get data ####
# ******************************************************************************
hc_req  <- read_excel(paste0("Data_dummy/", run_date, 
                             " Weekly Provider Order SL dummy.xlsx"), 
                      sheet=2, skip = 1) %>%
  clean_names()


# Lookups ####

# Postcode Lookup

postcode_lu <- readRDS(
  paste0(
    "/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/",
    "Scottish_Postcode_Directory_2021_2.rds")) %>%
  clean_names() %>%
  select(pc7, pc8, latitude, longitude, datazone2011)

# Locality lookup
# ******************************************************************************
locality_lu <- readRDS(
  paste0(
    "/conf/linkage/output/lookups/Unicode/Geography/HSCP Locality/",
    "HSCP Localities_DZ11_Lookup_20200825.rds"
  )) %>%
  clean_names() %>%
  select(hscp_locality, datazone2011)

hscp_loc_lu <- readRDS(
  paste0(
    "/conf/linkage/output/lookups/Unicode/Geography/HSCP Locality/",
    "HSCP Localities_DZ11_Lookup_20200825.rds"
  )) %>%
  clean_names() %>%
  select(hscp2019name, hscp_locality) %>% unique()

# hscp_loc_lu %>% filter(hscp2019name == "South Lanarkshire") %>% 
#   .[["hscp_locality"]] %>% shQuote() %>% paste0(collapse=",") %>% cat()


# Filtering out postcodes from the data
postcode_data <- select(hc_req, postcode) %>%
  unique()

# Create function to insert a space into the postcode when missing

fun_insert <- function(x, pos) {
  gsub(paste0("^(.{", pos, "})(.*)$"),
       paste0("\\1", " ", "\\2"),
       x)
}

# Filtering out postcodes without a space

nospace <- postcode_data %>%
  filter(!grepl(" ", postcode))

# Count num chars in postcode to work out where the space needs to go

nospace6 <- filter(nospace, nchar(postcode) == 6)
nospace7 <- filter(nospace, nchar(postcode) == 7)

# Test to check all postcodes without a space have 6 or 7 characters
n6_7 <- as.numeric(nrow(nospace6)) + as.numeric(nrow(nospace7))
nall <- as.numeric(nrow(nospace))

checks1 <- ifelse(n6_7 == nall, NA, "Issue! Postcode without 6/7 characters")

rm(n6_7, nall)


# Adding space
# ******************************************************************************
nospace6$postcode2 <- fun_insert(nospace6$postcode, pos = 3)
nospace7$postcode2 <- fun_insert(nospace7$postcode, pos = 4)

# Binding new postcodes dfs together
nospace = rbind(nospace6, nospace7)

postcode_data = left_join(postcode_data, nospace, by = "postcode") %>%
  mutate(postcode2 = case_when(!is.na(postcode2) ~ postcode2,
                               TRUE ~ postcode)) %>%
  unique()


# Manual recoding of issues
postcode_data <- postcode_data %>%
  # Removing additional space between two parts of postcode
  mutate(postcode2 = gsub(pattern = "  ", replacement = " ", x = postcode2),
         # Manually recoding issues
         postcode2 = recode(postcode2, "FK3 OAN" = "FK3 0AN"))

# Getting pc8 variable
post8 <- postcode_lu %>%
  select(-pc7) %>%
  rename(postcode2 = pc8)

# Joining long and lat data to our postcodes
postcode_lookup <- left_join(postcode_data, post8, by = "postcode2")

# Getting ones that haven't properly converted to manually fix issues above
na_pc <- postcode_lookup %>% filter(is.na(longitude) | is.na(latitude))


# Checks
if (nrow(na_pc) > 0) {
  checks2 = "Issue! Postcode coding error, needs manually fixed, missing long/lat."
} else checks2 = NA

# Joining postcode lookup to main datafile
hc_req <- left_join(hc_req, postcode_lookup, by = "postcode")
hc_req <- left_join(hc_req, locality_lu, by = "datazone2011")





# _________________ ####
#### Data Processing ####
###### Some of this section applies to Falkirk data anomolies and may not be required (lines 131-184)  #########

hc_req2 <- hc_req %>%

  # Removing data missing long/lat
  filter(!is.na(longitude)) %>%

  # R is annoyingly reading in the time data as an exponant. This is the easiest way to fix.
  mutate(am_call = case_when(am_call == "1.0416666666666666E-2" ~ "00:15",
                             am_call == "2.0833333333333332E-2" ~ "00:30",
                             am_call == "3.125E-2" ~ "00:45",
                             am_call == "4.1666666666666664E-2" ~ "01:00",
                             am_call == "6.25E-2" ~ "01:30",
                             am_call == "N/A" ~ "NA",
                             am_call == "n/a" ~ "NA",
                             TRUE ~ am_call),


         lunch_call = case_when(lunch_call == "1.0416666666666666E-2" ~ "00:15",
                                lunch_call == "2.0833333333333332E-2" ~ "00:30",
                                lunch_call == "3.125E-2" ~ "00:45",
                                lunch_call == "4.1666666666666664E-2" ~ "01:00",
                                lunch_call == "6.25E-2" ~ "01:30",
                                lunch_call == "N/A" ~ "NA",
                                lunch_call == "n/a" ~ "NA",
                                TRUE ~ lunch_call),


         tea_call = case_when(tea_call == "1.0416666666666666E-2" ~ "00:15",
                              tea_call == "2.0833333333333332E-2" ~ "00:30",
                              tea_call == "3.125E-2" ~ "00:45",
                              tea_call == "4.1666666666666664E-2" ~ "01:00",
                              tea_call == "6.25E-2" ~ "01:30",
                              tea_call == "N/A" ~ "NA",
                              tea_call == "n/a" ~ "NA",
                              TRUE ~ tea_call),


         beddown_call = case_when(beddown_call == "1.0416666666666666E-2" ~ "00:15",
                                  beddown_call == "2.0833333333333332E-2" ~ "00:30",
                                  beddown_call == "3.125E-2" ~ "00:45",
                                  beddown_call == "4.1666666666666664E-2" ~ "01:00",
                                  beddown_call == "6.25E-2" ~ "01:30",
                                  beddown_call == "N/A" ~ "NA",
                                  beddown_call == "n/a" ~ "NA",
                                  TRUE ~ beddown_call),

         # Number of visits is plus 1 for any value that isn't NA
         number_of_visits = 0,
         number_of_visits = ifelse(am_call != "NA", number_of_visits+1, number_of_visits),
         number_of_visits = ifelse(lunch_call != "NA", number_of_visits+1, number_of_visits),
         number_of_visits = ifelse(tea_call != "NA", number_of_visits+1, number_of_visits),
         number_of_visits = ifelse(beddown_call != "NA", number_of_visits+1, number_of_visits))


# Identify postcodes that have multiple requests within them
# These long/lats need slightly modified in order to make all icons viewable

multi_postcode <- hc_req2 %>%
  group_by(postcode2) %>%
  summarise(num = n()) %>%
  filter(num>1)

multi_postcode2 <- hc_req2 %>%
  left_join(., multi_postcode, by = "postcode2") %>%
  filter(!is.na(num))

repeat {

  # Getting number of unique rows for break condition for repeat loop
  n_unique_lon_lat <- multi_postcode2 %>%
    select(longitude, latitude) %>%
    unique()

  # Number of rows - break when equal to number of rows in multi postcode data
  n_row_before <- nrow(n_unique_lon_lat)

  multi_postcode2 <- multi_postcode2 %>%
    mutate(doub = ifelse(longitude == lag(longitude) & latitude == lag(latitude), 1, 0)) %>%
    replace_na(list(doub = 0)) %>%
    mutate(longitude = ifelse(doub == 1, longitude+.00008, longitude),
           latitude = ifelse(doub == 1, latitude+.00008, latitude))

  # After manipulation
  n_unique_lon_lat <- multi_postcode2 %>%
    select(longitude, latitude) %>%
    unique()

  n_row_after <- nrow(n_unique_lon_lat)

  if (n_row_before == n_row_after){
    break
  }
}


# Replacing the old data with the new data for offset long/lat

hc_req3 <- hc_req2 %>%
  left_join(., multi_postcode, by = "postcode2") %>%
  filter(is.na(num))

if(nrow(hc_req3) + nrow(multi_postcode2) == nrow(hc_req2)) {
  checks3 = NA
} else {
  checks3 = "Issue! The number of rows removed does not equal the number of rows changed for multiple postcodes. Check the return loop section and hc_req2 and 3. "
}

multi_postcode2 <- multi_postcode2 %>% select(-doub)

hc_req3 <- rbind(hc_req3, multi_postcode2) %>% select(-num)


# Data Processing Part 2 ####

hc_req3 <- hc_req3 %>%

  # Fixing strings to title format
  mutate(specific_gender_carer = str_to_title(specific_gender_carer),
         type_of_referral = str_to_title(type_of_referral),
         providers_comments_start_date = str_to_title(providers_comments_start_date)) %>%

  # Turning NA strings into recognised missing values
  replace_with_na(replace = list(additional_call_s = c("n/a", "N/A"),
                                 am_call = "NA",
                                 lunch_call = "NA",
                                 tea_call = "NA",
                                 beddown_call = "NA",
                                 specific_gender_carer = "N/A")) %>%

  # Creating urgent column
  mutate(urgent = case_when(providers_comments_start_date == "Urgent" |
                              providers_comments_start_date == "Urgnent" |
                              providers_comments_start_date == "Urgnet" ~ 1,
                            TRUE ~ 0),

         # Keeping only numeric answers for number of carers
         # This will introduce NA values by coercion - That's intended!
         no_of_carers = suppressWarnings(as.numeric(no_of_carers)),

         # Recoding Type of Referral into 3 categories
         # Summerford House - care home in FV - considered Community
         type_of_referral = case_when(grepl("Commu", type_of_referral) ~ "Community Referral",
                                      grepl("Homecare", type_of_referral) ~ "Homecare Intake Team",
                                      grepl("Hosp", type_of_referral) ~ "Hospital Referral",
                                      type_of_referral == "Discharge From Summerford House" ~ "Community Referral"))










# Data checks ####

# Prints strings to check data issues that might come up

print_issue_function <- function(checks) {
  if (!is.na(checks)){
    print(checks)
  }
}


print_issue_function(checks1)
print_issue_function(checks2)
print_issue_function(checks3)


# _________________ ####
#### Mapping setup ####

# ******************************************************************************
# Reading in locality shape file
# shape_loc_all <- readOGR(
#   dsn="/conf/linkage/output/lookups/Unicode/Geography/Shapefiles/Localities/", 
#   layer="Localities2016_MHW",
#   GDAL1_integer64_policy = T)
shape_loc_all <- readOGR(
  dsn="/conf/linkage/output/lookups/Unicode/Geography/Shapefiles/Localities/HSC Locality (Datazone2011 Base)/", 
  layer="HSC_Locality_Bdry",
  GDAL1_integer64_policy = T)
# shape_loc_all@data %>% colnames()
# shape_loc_all@data %>% class()
# shape_loc_all@data %>% View()
shape_loc_all@data %>% filter(locality %in% locality_vector)
head(shape_loc_all@data)
# shape_loc_all@data$OBJECTID

# Getting Falkirk localities from shape file
# length(shape_loc_all)
shape_falk_locs <- shape_loc_all[shape_loc_all@data$locality %in% locality_vector,]


# Setting colour palette for locality shapes
shape_cols <- colorFactor(palette = c("#8da0cb" , "#fc8d62", "#66c2a5", "#66c2a4"),
                          levels = locality_vector)

# Falkirk colours
falk_red = "#F26C45"
falk_teal = "#5bc5db"



# Creating labels for the tooltip for visit times
hc_req3 = hc_req3 %>%
  mutate(am_lab = ifelse(!is.na(am_call), "AM", "rmv"),
         lunch_lab = ifelse(!is.na(lunch_call), "Lunch", "rmv"),
         tea_lab = ifelse(!is.na(tea_call), "Tea", "rmv"),
         beddown_lab = ifelse(!is.na(beddown_call), "Bed", "rmv"),
         visit_lab = paste(am_lab, lunch_lab, tea_lab, beddown_lab, sep = ", "),
         visit_lab = gsub(pattern = "rmv, ", replacement = "", visit_lab),
         visit_lab = gsub(pattern = ", rmv", replacement = "", visit_lab)) %>%
  select(-am_lab, -lunch_lab, -tea_lab, -beddown_lab)

# Specific gender label
hc_req3 = hc_req3 %>%
  mutate(sp_gen_lab = case_when(!is.na(specific_gender_carer) ~ paste0("<b>Specific Gender:</b> ", specific_gender_carer, "<br>"),
                                TRUE ~ ""),
         # Urgent label
         urgent_label = case_when(urgent==1 ~ "<b> Urgent Request!</b> <br>",
                                  TRUE ~ ""),
         # Number of carers label
         ncar_lab = case_when(is.na(no_of_carers) ~ "",
                              TRUE ~ paste0("<br> <b> Number of Carers: </b> ", no_of_carers)))


# Tool tip
tooltip = paste0(hc_req3$urgent_label,
                 "<b>Order Number:</b> ", hc_req3$order_number,
                 "<br> <b> Request Date:</b> ", hc_req3$date_order_received_by_cmt,
                 "<br> <b> Visit Times:</b> ", hc_req3$visit_lab,
                 "<br>", hc_req3$sp_gen_lab,
                 "<b>Referral Type:</b> ", hc_req3$type_of_referral,
                 "<br> <b>Postcode:</b> ", hc_req3$postcode2,
                 "<br> <b>Days per Week:</b> ", hc_req3$days,
                 "<br> <b>Hours per Week:</b> ", hc_req3$hours,
                 hc_req3$ncar_lab)






# Setting icon colour factor
icon_cols = colorFactor(palette = c("red", "darkgreen"),
                        levels = c(0, 1))




# hc_req3 = hc_req3 %>%
#   mutate(group = as.factor(urgent))
#
# house_icons <- iconList("0" = makeIcon("/conf/LIST_analytics/Forth Valley/Falkirk/HomeCare/Mapping/Ruben 2021/Extras/tealhouse.png",
#                                     iconWidth = 32, iconHeight = 32),
#                        "1" = makeIcon("/conf/LIST_analytics/Forth Valley/Falkirk/HomeCare/Mapping/Ruben 2021/Extras/red house.png",
#                                     iconWidth = 32, iconHeight = 32))
#
#



### This section only needed for the data table tab of the dashboard summarising the outstanding package requests ###
### Rows 388 - 566 ###


# _________________ ####
#### Tables ####

# Setting border style for flex table
border_table = fp_border_default(color="black", width=1.5)

# PHS Purple
col_purple = "#9b4393" # Purple/Magenta

# * Number of requests: Locality ####

tab_loc_n = hc_req3 %>%
  group_by(hscp_locality) %>%
  summarise(requests = n()) %>%
  adorn_totals()

names(tab_loc_n) = c("Locality", "Number of Requests")


tab_loc_n_ft = flextable(tab_loc_n) %>%
  theme_vanilla() %>%
  bg(part = "header", bg = col_purple) %>%
  bg(i = 2, bg = "lightgray") %>%
  border_inner(border=border_table) %>%
  border_outer(border=border_table) %>%
  font(part = "all", fontname = "Arial") %>%
  fontsize(size = 12, part = "body") %>%
  fontsize(size = 12, part = "header") %>%
  width(width = 5) %>%
  color(part="header", color = "white") %>%
  color(part="body", color = "black") %>%
  bold()%>%
  align(part = "all", align = "left")

# * Urgent Requests ####

tab_urg = hc_req3 %>%
  group_by(urgent, hscp_locality) %>%
  summarise(req = n()) %>%
  mutate(urgent = recode(urgent, "0" = "Not Urgent", "1" = "Urgent")) %>%
  pivot_wider(names_from = urgent, values_from = req) %>%
  adorn_totals() %>%
  pivot_longer(cols = c("Not Urgent", "Urgent"), names_to = "Start Date", values_to = "req") %>%
  replace_na(list(req=0)) %>%
  group_by(hscp_locality) %>%
  mutate(perc = paste0(format(round_half_up(req/sum(req)*100, 1), nsmall = 1), "%"))

names(tab_urg) = c("Locality", "Urgent?", "Number of Requests", "Percent")


tab_urg_ft = flextable(tab_urg) %>%
  theme_vanilla() %>%
  bg(part = "header", bg = col_purple) %>%
  bg(i = 3:4, bg = "lightgray") %>%
  border_inner(border=border_table) %>%
  border_outer(border=border_table) %>%
  font(part = "all", fontname = "Arial") %>%
  fontsize(size = 12, part = "body") %>%
  fontsize(size = 12, part = "header") %>%
  width(width = 5) %>%
  color(part="header", color = "white") %>%
  color(part="body", color = "black") %>%
  bold()%>%
  align(part = "all", align = "left")


# * Number of Hours ####

tab_hours = hc_req3 %>%
  mutate(hours = as.numeric(hours)) %>%
  mutate(hour_band = case_when(hours <= 5 ~ "0 to 5 Hours",
                               hours <= 10 ~ "6 to 10 Hours",
                               hours <= 15 ~ "11 to 15 Hours",
                               hours <= 20 ~ "16 to 20 Hours",
                               hours <= 25 ~ "21 to 25 Hours",
                               hours <= 30 ~ "26 to 30 Hours",
                               hours > 30 ~ "More than 30 Hours"),
         hour_band = factor(hour_band, levels = c("0 to 5 Hours", "6 to 10 Hours","11 to 15 Hours","16 to 20 Hours",
                                                  "21 to 25 Hours","26 to 30 Hours","More than 30 Hours"))) %>%
  group_by(hour_band) %>%
  summarise(req=n()) %>%
  mutate(perc = paste0(format(round_half_up(req/sum(req)*100, 1), nsmall = 1), "%"))


names(tab_hours) = c("Locality", "Number of Requests", "Percent")


tab_hours_ft = flextable(tab_hours) %>%
  theme_vanilla() %>%
  bg(part = "header", bg = col_purple) %>%
  border_inner(border=border_table) %>%
  border_outer(border=border_table) %>%
  font(part = "all", fontname = "Arial") %>%
  fontsize(size = 12, part = "body") %>%
  fontsize(size = 12, part = "header") %>%
  width(width = 5) %>%
  color(part="header", color = "white") %>%
  color(part="body", color = "black") %>%
  bold()%>%
  align(part = "all", align = "left")


# * Number of Carers ####
class(hc_req3$no_of_carers)
tab_carers = hc_req3 %>%
  group_by(no_of_carers) %>%
  summarise(req = n()) %>%
  # the line below was failing as req is numeric - unclear if line required
  #replace_na(list(no_of_carers = "Unknown")) %>%
  mutate(perc = paste0(format(round_half_up(req/sum(req)*100, 1), nsmall = 1), "%"))

names(tab_carers) = c("Number of Carers", "Number of Requests", "Percent")


tab_carers_ft = flextable(tab_carers) %>%
  theme_vanilla() %>%
  bg(part = "header", bg = col_purple) %>%
  border_inner(border=border_table) %>%
  border_outer(border=border_table) %>%
  font(part = "all", fontname = "Arial") %>%
  fontsize(size = 12, part = "body") %>%
  fontsize(size = 12, part = "header") %>%
  width(width = 5) %>%
  color(part="header", color = "white") %>%
  color(part="body", color = "black") %>%
  bold()%>%
  align(part = "all", align = "left")


# * Number of Visits ####

tab_visits = hc_req3 %>%
  group_by(number_of_visits) %>%
  summarise(req = n()) %>%
  #replace_na(list(number_of_visits = "Unknown")) %>%
  mutate(perc = paste0(format(round_half_up(req/sum(req)*100, 1), nsmall = 1), "%"))

names(tab_visits) = c("Number of Visits Per Day", "Number of Requests", "Percent")


tab_visits_ft = flextable(tab_visits) %>%
  theme_vanilla() %>%
  bg(part = "header", bg = col_purple) %>%
  border_inner(border=border_table) %>%
  border_outer(border=border_table) %>%
  font(part = "all", fontname = "Arial") %>%
  fontsize(size = 12, part = "body") %>%
  fontsize(size = 12, part = "header") %>%
  width(width = 5) %>%
  color(part="header", color = "white") %>%
  color(part="body", color = "black") %>%
  bold()%>%
  align(part = "all", align = "left")


# * Days per Week ####

tab_days = hc_req3 %>%
  mutate(days = case_when(days %in% c("1", "2", "3", "4", "5", "6", "7") ~ days,
                          TRUE ~ "Variable")) %>%
  group_by(days) %>%
  summarise(req = n()) %>%
  replace_na(list(days = "Unknown")) %>%
  mutate(perc = paste0(format(round_half_up(req/sum(req)*100, 1), nsmall = 1), "%"))

names(tab_days) = c("Number of Days Per Week", "Number of Requests", "Percent")


tab_days_ft = flextable(tab_days) %>%
  theme_vanilla() %>%
  bg(part = "header", bg = col_purple) %>%
  border_inner(border=border_table) %>%
  border_outer(border=border_table) %>%
  font(part = "all", fontname = "Arial") %>%
  fontsize(size = 12, part = "body") %>%
  fontsize(size = 12, part = "header") %>%
  width(width = 5) %>%
  color(part="header", color = "white") %>%
  color(part="body", color = "black") %>%
  bold()%>%
  align(part = "all", align = "left")



# _________________ ####
#### DT ####


# Select columns of interest

filt_hc_2 = hc_req3 %>%
  mutate(date_order_received_by_cmt = ymd(date_order_received_by_cmt),
         month = month(date_order_received_by_cmt, label = TRUE),
         year = year(date_order_received_by_cmt),
         date2 = paste0(month, "-", year)) %>%
  select(-month, -year) %>%
  arrange(date_order_received_by_cmt)


filt_hc = filt_hc_2 %>%
  mutate(date2 = factor(date2, unique(date2)),
         tooltip = paste0(urgent_label,
                          "<b>Order Number:</b> ", order_number,
                          "<br> <b> Request Date:</b> ", date_order_received_by_cmt,
                          "<br> <b> Visit Times:</b> ", visit_lab,
                          "<br>", sp_gen_lab,
                          "<b>Referral Type:</b> ", type_of_referral,
                          "<br> <b>Postcode:</b> ", postcode2,
                          "<br> <b>Days per Week:</b> ", days,
                          "<br> <b>Hours per Week:</b> ", hours,
                          ncar_lab),
         providers_comments_start_date = case_when(urgent == 1 ~ "Urgent",
                                                   urgent == 0 ~ "Non-Urgent")) %>%
  select(-urgent, -urgent_label, -ncar_lab, -sp_gen_lab, -visit_lab)

#names(filt_hc)


# Creating icons
icons_home = awesomeIcons(icon = "fa-home", library = "fa", iconColor = "black", markerColor = "red")

#
# names(filt_hc) = c("Order Number", "Request Date", "SWIS No.", "Postcode", "Locality", "Referral Type", "Hours (Wk)",
#                    "No. Visits (Day)", "Days (Wk)", "Call Times", "Additional Calls", "Specific Times", "No. Carers",
#                    "Carer Gender", "Start Date: Urgent?", "Month-Year")

filt_hc2 = SharedData$new(filt_hc)

