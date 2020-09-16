  library(tidyverse)
  library(readxl) # for importing excel sheets
  library(chron) # for classing time field correctly
  library(googlesheets4) # for importing Google Sheets
  
  # Change working directory to your local R project folder
 setwd("/Users/bapu/Projects/watershed/action/public-safety/yolo/analysis/davis/")
  
  # IMPORT DATASETS----
  
    # Service calls 2015-2020. Raw dataset provided by the Davis PD.
  calls_raw <- lapply(
    excel_sheets("./data/raw/service-calls-raw.xlsx"), 
    read_excel, path = "./data/raw/service-calls-raw.xlsx")

    # Disposition codes
      # Read in codes
        # This Google Sheet is a categorization of disposition codes provided by 
        # the Davis Police Department. You will be asked to confirm email for 
        # Google sheets access. 
  disp_code <- read_sheet(
    ("https://docs.google.com/spreadsheets/d/1SrhDb-rxyqIBRAQBG6anMMDuILskZcbDtiO6iP7hgcw/edit?usp=sharing"))
      # Remove "Inactive" field
  disp_code <- disp_code[,1:2] 
      # Rename "Value" and "Description" fields as "disp_value" and "disp_desc" 
  colnames(disp_code) <- c("disp_value", "disp_desc")
  
    # Incident type codes
      # Read in codes
        # This Google Sheet is a categorization of incident type codes provided 
        # by the Davis Police Department. You will be asked to confirm email for 
        # Google sheets access. 
  incident_type <- read_sheet(
    "https://docs.google.com/spreadsheets/d/1svbcwdO2EqMoTQHGl5g37CjHaaQNzfceL1A6AjB2fCU/edit?usp=sharing",
    sheet = "new-categories")
      # Unlist data
  incident_type$type <- unlist(incident_type$type)
      # Make data a factor variable
  incident_type <- incident_type %>% mutate_all(as.factor)
  
  # PREPARE DATASETS----
  
    # Bind service call list elements into one data frame
  calls <- bind_rows(calls_raw)
    # Rename columns of new data frame
  colnames(calls) = c("inc_num", "date", "time", "type", "beat", "street",
                      "cross_str", "disp_value")
    # Join calls data with disposition codes
  calls_desc <- left_join(calls, disp_code, by = "disp_value")
    # Join calls/disposition codes with incident types
  calls_desc <- left_join(calls_desc, incident_type, by = "type")
    # Delete empty rows
  calls_desc <- calls_desc %>% filter_all(any_vars(!is.na(.)))
    # Correct column classes
  calls_desc <- calls_desc %>% mutate_at(vars(
    inc_num, type, beat, disp_value, disp_desc), as.factor)
    # Make time field a time class
  calls_desc$time <- times(calls_desc$time)
  
  # SUPPLEMENTAL TABLES FOR SHINY APP----
  
    # Create dataset counting observations in each service call category 
    # by date and disposition
  calls_sum <- calls_desc %>% count(category, date, disp_desc)
    
    # Group by date, category, and disposition
  calls_sum <- calls_sum %>%
    group_by(date, category, disp_desc) %>%
    summarize(n = sum(n))
      
    # Class date field correctly
  calls_sum$date <- as.Date(calls_sum$date)
  
    # Remove unknown
  calls_sum <- subset(calls_sum, category!= "Unknown")
  
    # Order factor levels by severity, according to Davis PD-provided scheme
  calls_sum <- calls_sum %>%
    mutate(category = factor(category, 
                              levels = c(
                                "Documentation Only/Miscellaneous",
                                "Online Crime Reporting",
                                "Nuisance/Code Enforcement",
                                "Traffic/Minor Vehicle Violations or Issues",
                                "Required Other Agency/Department Response",
                                "Officer-Initiated Contact",
                                "Property Theft/Destruction/Lost",
                                "Mental Health/Welfare",
                                "Drug/Alcohol",
                                "Patrol Response for Assessment",
                                "Traffic/Major Safety",
                                "Suspicious Activity",
                                "Violent Crime/Abuse or Neglect/Public Safety"
                              )
                             )
           )
  
    # Remove NA rows
  calls_sum <- calls_sum %>% drop_na(category)
  
  # EXPORT DATASETS----
  write.csv(calls_desc, "./data/generated/service-calls.csv")
  write.csv(calls_sum,"./data/generated/calls_sum.csv", row.names = F)
    # Shiny tables into shiny folder. May need to change based on location on
    # user's computer.
  write.csv(calls_sum,"./shiny/calls_sum.csv", row.names = F)
  

  
 


    