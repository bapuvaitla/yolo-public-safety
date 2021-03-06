# yolo-public-safety
Yolo County public safety analysis  

This repository contains data on public safety and social services in Yolo County, CA, including city-level data for Davis, West Sacramento, Winters, and Woodland. Each geographical unit contains the same subfolder structure, described below.

## data

The data folder contains two subfolders. The `raw` folder contains data and metadata obtained from the various departments using the Public Records Act. A list of what's currently included:  

_davis-arrest-log-raw_: Daily arrest logs from the Davis PD from June 2015 to June 2020.  
_codes_: A document from the Office of the Attorney General of California listing arrest offense codes grouped by felony, misdemeanor, and crime category. From https://oag.ca.gov/sites/all/files/agweb/pdfs/cjsc/prof10/codes.pdf    
_service-calls-raw_: Logs from the Davis PD of all service calls received between January 2015 and June 2020.  
_incident-type_: A document provided by the Davis PD to interpret incident types in the service calls dataset.  
_PRA PD Beat Map_: A document provided by the Davis PD to interpret beat numbers in the service calls dataset.  
  
The `generated` folder contains datasets generated by various analytical scripts.  

_davis_log_: A single dataframe binding all data scraped from the raw arrest log.  
_davis_log_long_: A reshaped long version of _davis_log_.  
_service-calls_: A tidied version of the raw service calls log.  
_race_totals_: Total arrests by arrest category and race.  
_race_cat_norm_: Arrests by category, normalized by population.  
_race_cat_pop_: Arrests by category, in comparison with population.  

## scripts  

_davis-arrest-log_: This script receives _davis_arrest_log_raw_ as input. It cleans, reorganizes, and joins the arrest log with several other data tables (mainly for categorizing crimes) from online sources.   
_davis-service-log_: This script receives _service-calls-raw_ as input. It cleans and reorganizes the service calls dataset, and joins it with a disposition code dataset stored online (originally received from the Davis PD).  
_visualizations_: This script reshapes the service call and arrest log datasets and generates visualizations.  
_supp-tables_: This script contains code for generating supplemental objects, including the datasets used by the Shiny app.

## shiny
_app_: Shiny app for visualizations. 
Data files for the Shiny app are also included in this folder.
