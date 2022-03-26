## -----------------------------------------------------------------------------
#'  1_importclean.R
#'
#' by: Chris Holiday
#'
#'  Preprocessing: importing, cleaning and merging all data 
#'
#'
## -----------------------------------------------------------------------------
## Managing dependencies
## -----------------------------------------------------------------------------
##rm(list = ls())

library(data.table)
library(stringr)
library(bit64)

## -----------------------------------------------------------------------------
## NDC codes to identify medications and Years for Data availability
## -----------------------------------------------------------------------------

ndc_apriprazole <- c("59148-0018-71", "59148-0019-71", "65757-401-03",
                     "65757-402-03",  "65757-403-03")

ndc_fluphenazine <- c("0143-9529", "42023-129", "42023-129", "55150-267", 
	                  "63323-272", "63323-281", "67457-359")

ndc_haloperidol <- c("0143-9295", "0143-9296", "0143-9319", "0143-9501", 
	"0143-9502",  "0703-7011",  "0703-7013",  "0703-7021",  "0703-7121", 
	"0703-7123",  "0703-7131",  "0703-7133",  "10147-0911", "10147-0921", 
	"10147-0922", "17478-110",  "25021-806",  "25021-831",  "25021-833", 
	"25021-834",  "50458-253",  "50458-254",  "50458-255",  "52584-426",
	"55154-7076", "55154-7478", "55154-9553", "63323-469",  "63323-471", 
	"63323-471",  "63323-474",  "67457-381",  "67457-409",  "67457-410", 
	"67457-426",  "68083-117",  "68083-137",  "68083-138",  "70069-030", 
	"70069-031",  "70069-381",  "70069-382",  "70069-383",  "70069-384", 
	"70518-0776", "70518-1053", "70518-1809", "70518-2520", "70710-1461", 
	"70710-1462", "70710-1463", "70710-1464", "72785-0004", "72785-0005",
	"72785-0006", "72785-0007", "76045-737")

ndc_olanzapine <- c("0002-07635-11", "0002-07636-11", "0002-07637-11",
	   "0002-7597", "0517-0955", "0781-3159","0781-9105")

ndc_paliperidone <- c("50458-560-01", "50458-561-01", "50458-562-01", 
	                  "50458-563-01", "50458-564-01", "50458-606-01", 
	                  "50458-607-01", "50458-608-01", 
					  "50458-609")

ndc_risperidone <- c("12496-0090",   "12496-0120",   "50458-306-11", 
	                 "50458-307-11", "50458-308-11", "50458-309-11")



ndc_lai <- c(ndc_apriprazole, ndc_fluphenazine, ndc_haloperidol, 
	         ndc_olanzapine,  ndc_paliperidone, ndc_risperidone)


years <- c(2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010,
	       2011, 2012, 2013, 2014, 2015, 2016, 2017)

## -----------------------------------------------------------------------------
## Import and clean Hcup data
## -----------------------------------------------------------------------------


hcup_raw_ <-fread(file.path(raw, paste0("HCUP_", years[1], ".csv")),
 			  sep = ",")

colnames(hcup_raw_) <- c("State", "Payer", "discharge", "perc_discharge", "LOS", "charge", "V7")

## Restrict to rows that include state names and Medicaid data, extract data ##
hcup_raw_ <- hcup_raw_[str_detect(State, "All Codes Combined")  | Payer == "Medicaid" ]
hcup_raw_$State <- shift(str_extract(str_extract(hcup_raw_$State, "(^([^,]*))"), "(?<=\\s).*"))
hcup_raw_ <- hcup_raw_[! is.na(State) & ! State == "Combined"]
hcup_raw_ <- hcup_raw_[State != "Type: Descriptive Statistics | Setting of Care: Hospital Inpatient | Geographic Settings: State | Subgroups of Interest: All Codes Combined"]
hcup_raw_$Year <- years[1]
hcup_raw <- hcup_raw_[, c("State", "discharge", "LOS", "charge", "Year")]

## Loop over all years ##
for (yr in years[-1]) {
hcup_raw_ <-fread(file.path(raw, paste0("HCUP_", yr, ".csv")),
 			  sep = ",")

colnames(hcup_raw_) <- c("State", "Payer", "discharge", "perc_discharge", "LOS", "charge", "V7")
hcup_raw_ <- hcup_raw_[str_detect(State, "All Codes Combined") | Payer == "Medicaid" ]
hcup_raw_$State <- shift(str_extract(str_extract(hcup_raw_$State, "(^([^,]*))"), "(?<=\\s).*"))
hcup_raw_ <- hcup_raw_[! is.na(State) & ! State == "Combined"]
hcup_raw_ <- hcup_raw_[State != "Type: Descriptive Statistics | Setting of Care: Hospital Inpatient | Geographic Settings: State | Subgroups of Interest: All Codes Combined"]
hcup_raw_$Year <- yr

hcup_raw <- rbind(hcup_raw, hcup_raw_[, c("State", "discharge", "LOS", "charge", "Year")])
}

## Rename State Names, remove leading space ##
hcup_raw$State <- state.abb[match(gsub("^\\s", "", hcup_raw$State), state.name)]

## Convert Numeric columns to Numeric ##
hcup_raw$n      <- as.numeric(as.character(hcup_raw$discharge))
hcup_raw$LOS    <- as.numeric(as.character(hcup_raw$LOS))
hcup_raw$charge <- as.numeric(as.character(hcup_raw$charge))


## -----------------------------------------------------------------------------
## Import and Clean CPI data
## -----------------------------------------------------------------------------

cpi_raw <- fread(file.path(raw, "CPIAUCSL.csv"), sep = ",", header = TRUE) 
cpi_raw$Year <- as.numeric(substring(cpi_raw$DATE, 1, 4))
cpi_ <- cpi_raw[cpi_raw$Year >= 2001 & cpi_raw$Year <= 2017]
cpi <- aggregate(x = cpi_[,!c("DATE")],
	             by = list(cpi_$Year),
	             FUN = mean)

cpi$CPI <- cpi$CPIAUCSL / cpi$CPIAUCSL[cpi$Year == 2017] * 100
cpi$CPIAUCSL <- NULL
cpi$Group.1 <- NULL

## -----------------------------------------------------------------------------
## Merge and fix average cost to CPI adjusted
## -----------------------------------------------------------------------------

hcup_raw <- merge(x = hcup_raw, y = cpi, by = c("Year"), all.x = TRUE)
hcup_raw$charge <- hcup_raw$charge * (100 / hcup_raw$CPI)

## -----------------------------------------------------------------------------
## Import and Clean population data
## -----------------------------------------------------------------------------

enrol_raw <-fread(file.path(raw, "KFF_2008_2019.csv"), 
 	sep = ",", header = TRUE)

## Melt Data ##
colnames(enrol_raw) <- c("State", "2008", "2009", "2010", "2011", "2012", "2013", 
	"2014", "2015", "2016", "2017", "2018", "2019")
enrol_long <- melt(enrol_raw, id.vars = c("State"), measure.vars = c("2008", "2009", 
	"2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019"))

## Convert State Names ##
enrol_long$State <- state.abb[match(enrol_long$State, state.name)]
colnames(enrol_long) <- c("State", "Year", "medicaid_enrol")

## year column causing merge issues
hcup_raw$Year2 <- as.double(hcup_raw$Year)
enrol_long[, Year := as.character(Year)]
hcup_raw[, Year := as.character(Year)]

## Merge to HCUP ##
hcup <- merge(x = hcup_raw, y = enrol_long, by = c("State", "Year"), 
 	              all.x= TRUE)


hcup <- hcup[, Year := as.double(Year)]

## -----------------------------------------------------------------------------
## Import and Clean unemployment data
## -----------------------------------------------------------------------------

## import and merge ##
unemp1_raw <- fread(file.path(raw, "unemp_az_to_mi.csv"), sep = ",")
unemp2_raw <- fread(file.path(raw, "unemp_ne_to_wi.csv"), sep = ",")
unemp_raw  <- merge(x = unemp1_raw, 
	                y = unemp2_raw, 
	               by = c("DATE"))

## Define and aggregate across year ##
unemp_raw$Year <- substring(unemp_raw$DATE, 1, 4)
unemp <- aggregate(x = unemp_raw[, !c("DATE")],
                  by = list(unemp_raw$Year), 
                 FUN = mean)

## Define colnames ##
colnames(unemp) <- c("Year", "AZ", "CA", "CO", "FL", "HI", "IA", "KS", "KY", "MA", 
	"MI", "MN", "MO", "NC", "NE", "NJ", "NY", "OR", "SC", "TN", "UT", "VT", "WA", "WV", "WI", 
	"Drop")
unemp$Drop <- NULL

unemp <- as.data.table(unemp)
## Melt and merge to HCUP ##
unemp <- melt(unemp, id.vars = c("Year"), measure.vars = c("AZ", "CA", "CO", "FL", "HI", "IA", 
	"KS", "KY", "MA", "MI", "MN", "MO", "NC", "NE", "NJ", "NY", "OR", "SC", "TN", 
	"UT", "VT", "WA", "WV", "WI"))
colnames(unemp) <- c("Year", "State", "unemp")
unemp <- unemp[, Year := as.double(Year)]
hcup <- merge(x = hcup, y = unemp, by = c("State", "Year"), 
 	              all.x= TRUE)

## -----------------------------------------------------------------------------
## Creating legalization dummy variable
## -----------------------------------------------------------------------------

hcup$legalization <- 0 
hcup[State == "CO" & Year >= 2012, "legalization"] <- 1
hcup[State == "WA" & Year >= 2012, "legalization"] <- 1
hcup[State == "MN" & Year >= 2014, "legalization"] <- 1
hcup[State == "OR" & Year >= 2014, "legalization"] <- 1
hcup[State == "CA" & Year >= 2016, "legalization"] <- 1
hcup[State == "MA" & Year >= 2016, "legalization"] <- 1

## -----------------------------------------------------------------------------
## Import and Clean State Drug Utilization Data
## -----------------------------------------------------------------------------

## Remove dashes from NDC codes ##
ndc_lai_concat <- paste(gsub(pattern = "-", "", x = ndc_lai), collapse = "|^")

## Import Data and Restrict To Relevant Medications, States, then collapse ##
SDUD_raw_ <-fread(file.path(raw, 
	paste0("State_Drug_Utilization_Data_2017.csv")), sep = ",")
SDUD_raw_ <- SDUD_raw_[State %in% c("AZ", "CA", "CO", "FL", "HI",
	"IA", "KS", "KY", "MA", "MI", "MN", "MO", "NE", "NJ", "NY", "NC", "OR", "SC", 
	"TN", "UT", "VT", "WA", "WV", "WI")]
SDUD_raw <- SDUD_raw_[str_detect(NDC, ndc_lai_concat), 
                      c("State", "Number of Prescriptions", "Units Reimbursed")]

if (dim(SDUD_raw)[1] > 0){
SDUD_collap <- aggregate(cbind(`Number of Prescriptions`, `Units Reimbursed`) ~ State, data = SDUD_raw, FUN=sum)
SDUD_collap$Year <- 2017
SDUD <- SDUD_collap
}

## Limiting to 2003 for the first approval of a LAI by FDA and looping over available years ##
for (yr in c(2016, 2015, 2014, 2013, 2012, 2011, 2010, 2009, 2008, 2007, 2006, 2005, 
	         2004, 2003)) {

## Import, restrict to states and medications
SDUD_raw_ <-fread(file.path(raw, 
	paste0("State_Drug_Utilization_Data_", yr, ".csv")), sep = ",")
SDUD_raw_ <- SDUD_raw_[State %in% c("AZ", "CA", "CO", "FL", "HI",
	"IA", "KS", "KY", "MA", "MI", "MN", "MO", "NE", "NJ", "NY", "NC", "OR", "SC", 
	"TN", "UT", "VT", "WA", "WV", "WI")]
SDUD_raw <- SDUD_raw_[str_detect(NDC, ndc_lai_concat), 
                      c("State", "Number of Prescriptions", "Units Reimbursed")]
print(yr)
## This is a logical check to make sure we only aggregate if prescriptions were found ##
if (dim(SDUD_raw)[1] > 0){

SDUD_collap <- aggregate(cbind(`Number of Prescriptions`,`Units Reimbursed`) ~ State, data = SDUD_raw, FUN=sum)
SDUD_collap$Year <- yr

if (exists("SDUD")){
SDUD <- rbind(SDUD, SDUD_collap)
} else {
SDUD <- SDUD_collap
}
}
}

## Variable Renaming ##
colnames(SDUD) <- c("State", "n_scripts", "N_LAI", "Year")

## -----------------------------------------------------------------------------
## Merge HCUP with State Drug Utilization Data
## -----------------------------------------------------------------------------

lai_data <- merge(x = hcup, y = SDUD, by = c("State", "Year"),  all.x= TRUE)

## Add LAI_per_enrol ##
lai_data$scripts_cap <- (lai_data$n_scripts / lai_data$medicaid_enrol) * 1000
lai_data$LAI_cap     <- (lai_data$N_LAI / lai_data$medicaid_enrol) * 1000

## Fixing missing values ##
lai_data[is.na(n_scripts),   "n_scripts"]   <- 0
lai_data[is.na(scripts_cap), "scripts_cap"] <- 0
lai_data[is.na(N_LAI),       "N_LAI"]       <- 0
lai_data[is.na(LAI_cap),     "LAI_cap"]     <- 0

write.csv(lai_data, file.path(output, paste0("lai_data_", Sys.Date(), ".csv")))

## -----------------------------------------------------------------------------
