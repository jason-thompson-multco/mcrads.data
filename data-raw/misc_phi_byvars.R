# Author: Danny Colombara
# R version: 4.2.1
# Date: March 15, 2023
# Purpose: Refresh standard phi cat/varname/group/group_alias from SharePoint
# Source: SharePoint >> Community Health Indicators >> phi-Vizes >> phi-Standards-TableauReady Output.xlsx
# Notes:

# Set up ----
rm(list=ls())
library(data.table)
library(stringi)
library(mcrads.data)

# Import data from SharePoint ----
#    team <- get_team("Community Health Indicators")
#
#    drv <- team$get_drive("phi-Vizes")
#
#    tempy <- tempfile(fileext = ".xlsx")
#
#    drv$download_file(src = 'phi-Standards-TableauReady Output.xlsx',
#                      dest = tempy,
#                      overwrite = FALSE)

    file_url <- "https://raw.githubusercontent.com/jason-thompson/mcrads.data/main/data/misc_phi_byvars.rda"

    con <- url(file_url, "rb")

    load(con)

    close(con)

    misc_phi_byvars <- copy(my_data)

    mcrads::sql_clean(misc_phi_byvars) # get rid of misc whitespace

    misc_phi_byvars <- as.data.table(misc_phi_byvars)

    

    setorder(misc_phi_byvars, cat, varname, group)
    misc_phi_byvars <- rbind(misc_phi_byvars[cat == 'Multnomah County'], misc_phi_byvars[cat == "Oregon State"], misc_phi_byvars[!cat %in% c("Multnomah County", "Oregon State")])
    misc_phi_byvars[, creation_date := Sys.Date()]

    misc_phi_byvars[, notes := gsub('"', "`", notes)] # Replace all quotation marks with tick marks

# Tidy irregular whitespaces ----
    mcrads::sql_clean(misc_phi_byvars)
    string.columns <- names(misc_phi_byvars) # all are strings
    misc_phi_byvars[, (string.columns) := lapply(.SD, function(x){stringi::stri_replace_all_charclass(x, "\\p{WHITE_SPACE}", " ")}), .SDcols = string.columns] # replace irregular whitespaces with true whitespaces (' ')

# Identify differences since the previous run ----
  existing <- fread('https://raw.githubusercontent.com/jason-thompson/mcrads.data/main/inst/extdata/misc_data/phi_byvars.csv')
  setorder(existing, cat, varname, group)
  misc_phi_byvars <- rbind(misc_phi_byvars[cat == 'Multnomah County'],
                           misc_phi_byvars[cat == "Oregon State"],
                           misc_phi_byvars[!cat %in% c("Multnomah County", "Oregon State")]) # to force MC and Wa to be at the top


  if(nrow(fsetdiff(misc_phi_byvars[, 1:5], existing[,1:5])) == 0 && nrow(fsetdiff(existing[,1:5], misc_phi_byvars[, 1:5])) == 0){
    message('There reference table has not been updated so rads.data will not be updated.')
  } else {
    message("The following rows are not in the pre-existing data and will be added:")
    print(fsetdiff(misc_phi_byvars[, 1:5], existing[, 1:5]))

    message("The following rows are in the pre-existing data but not in the new data ... they will be dropped:")
    print(fsetdiff(existing[, 1:5], misc_phi_byvars[, 1:5]))

    answer <- readline(prompt = "Are you ABSOLUTELY POSITIVE you want to continue? (y/n) ")
    if(answer == 'y'){
      # Save as RDA file ----
      usethis::use_data(misc_phi_byvars, compress = "bzip2", version = 3, overwrite = TRUE)

      # Save as CSV file -----
      write.csv(misc_phi_byvars, file = "inst/extdata/misc_data/phi_byvars.csv", row.names = FALSE)

      message('The updated data was written to rads.data.\n Update the helpfile if needed.')
    }
    if(answer == 'n'){
      message("There were changes compared to the last run, but you have decided not to update the data.")
    }
  }

# The end ----
