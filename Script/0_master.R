## -----------------------------------------------------------------------------
#'  0_master.R
#'
#' by: Chris Holiday
#'
#'  A master script which runs data cleaning, analysis and model files. Resulting
#'  output can be found in "output"
#'
#'
## -----------------------------------------------------------------------------
## Managing dependencies
## -----------------------------------------------------------------------------

library(data.table)

## -----------------------------------------------------------------------------
## Setting directory paths
## -----------------------------------------------------------------------------

raw    <- "/home/choliday/ECO 2408/LAI_Utilization/Raw"
temp   <- "/home/choliday/ECO 2408/LAI_Utilization/Temp"
script <- "/home/choliday/ECO 2408/LAI_Utilization/Script"
output <- "/home/choliday/ECO 2408/LAI_Utilization/Output"

## -----------------------------------------------------------------------------
## Running Scripts
## -----------------------------------------------------------------------------

 source(file.path(script, "1_importclean.R"))
 source(file.path(script, "2_graphs.R"))
 source(file.path(script, "3_models.R"))
 source(file.path(script, "4_diagnostics_statistics.R"))

## -----------------------------------------------------------------------------
