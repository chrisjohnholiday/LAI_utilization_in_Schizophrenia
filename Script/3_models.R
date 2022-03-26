## -----------------------------------------------------------------------------
#'  3_models.R
#'
#' by: Chris Holiday
#'
#'  Runs models and outputs LaTeX
#'
#'
## -----------------------------------------------------------------------------
## Managing dependencies
## -----------------------------------------------------------------------------

library(plm)
library(stargazer)
library(sandwich)

## -----------------------------------------------------------------------------
## Import data and clean up data types
## -----------------------------------------------------------------------------

lai_data <- read.csv(file.path(output, "lai_data_2020-11-26.csv"))

lai_data$discharge <- as.numeric(lai_data$discharge)
lai_data$Year2 <- as.factor(lai_data$Year2)

## -----------------------------------------------------------------------------
## Pooled OLS Models
## -----------------------------------------------------------------------------

discharge_ols <- plm(discharge ~ N_LAI + unemp + legalization + Year2, 
	                  data = lai_data,
	                 index = c("State", "Year"),
	                 model = "pooling")
LOS_ols       <- plm(LOS ~ LAI_cap + unemp + legalization + Year2, 
	                  data = lai_data,
	                 index = c("State", "Year"),
	                 model = "pooling")
charge_ols    <- plm(charge ~ LAI_cap + unemp + legalization + Year2, 
	                  data = lai_data,
	                 index = c("State", "Year"),
	                 model = "pooling")

## -----------------------------------------------------------------------------
## Random Effects Models
## -----------------------------------------------------------------------------

discharge_re <- plm(discharge ~ N_LAI + unemp + legalization + Year2, 
	                  data = lai_data,
	                 index = c("State", "Year"),
	                 model = "random",
	         random.method = "walhus")

LOS_re       <- plm(LOS ~ LAI_cap + unemp + legalization + Year2, 
	                  data = lai_data,
	                 index = c("State", "Year"),
	                 model = "random",
	         random.method = "walhus")

charge_re    <- plm(charge ~ LAI_cap + unemp + legalization + Year2, 
	                  data = lai_data,
	                 index = c("State", "Year"),
	                 model = "random",
	         random.method = "walhus")


## -----------------------------------------------------------------------------
## Fixed Effects Models
## -----------------------------------------------------------------------------

discharge_fe <- plm(discharge ~ N_LAI + unemp + legalization + Year2, 
	                  data = lai_data,
	                 index = c("State", "Year"),
	                 model = "within")
LOS_fe       <- plm(LOS ~ LAI_cap + unemp + legalization + Year2, 
	                  data = lai_data,
	                 index = c("State", "Year"),
	                 model = "within")
charge_fe    <- plm(charge ~ LAI_cap + unemp + legalization + Year2, 
	                  data = lai_data,
	                 index = c("State", "Year"),
	                 model = "within")

## -----------------------------------------------------------------------------
## Hausman Tests
## -----------------------------------------------------------------------------

discharge_ht <- phtest(discharge_re, discharge_fe)
LOS_ht       <- phtest(LOS_re, LOS_fe)
charge_ht    <- phtest(charge_re, charge_fe)

## -----------------------------------------------------------------------------
## Stargazer by dependant variable
## -----------------------------------------------------------------------------

print(" \n \n \n \n Number of Hospital Discharges")
stargazer(discharge_ols, discharge_re, discharge_fe, 
                    se = list(coef(summary(discharge_ols, cluster = c("State")))[,2], 
    	                      coef(summary(discharge_re,  cluster = c("State")))[,2], 
       	                      coef(summary(discharge_fe,  cluster = c("State")))[,2]), 
                 title = "Number of Hospital Discharges", 
        dep.var.labels = c("Discharges"),
         column.labels = c("Pooled OLS" , "Random Effects", "Fixed Effects"), 
      covariate.labels = c("Units of LAI Reimbursed",
       	                   "Unemployment Rate",
       	                   "Legalization"),
       	        digits = 2, 
       	     add.lines = list(c("Year Effects", "Yes", "Yes", "Yes"), 
       	           	          c("Standard Errors", 
       	     	               "Clustered", "Clustered", "Clustered")),
       	      no.space = TRUE, 
       	          omit = "Year2", 
       	     omit.stat = c("ser"), 
         model.numbers = FALSE, 
             star.char = c("", "", ""), 
     omit.table.layout = "n")


print(" \n \n \n \n Average Length of Stay (LOS)")
stargazer(LOS_ols, LOS_re, LOS_fe, 
                    se = list(coef(summary(LOS_ols, cluster = c("State")))[,2], 
    	                      coef(summary(LOS_re,  cluster = c("State")))[,2], 
       	                      coef(summary(LOS_fe,  cluster = c("State")))[,2]), 
                 title = "Avgerage Length of Stay (LOS)", 
        dep.var.labels = c("LOS"),
         column.labels = c("Pooled OLS" , "Random Effects", "Fixed Effects"), 
      covariate.labels = c("Units per 1000 Medicaid",
       	                   "Unemployment Rate",
       	                   "Legalization"),
       	        digits = 2, 
       	     add.lines = list(c("Year Effects", "Yes", "Yes", "Yes"), 
       	           	          c("Standard Errors", 
       	     	               "Clustered", "Clustered", "Clustered")),
       	      no.space = TRUE, 
       	          omit = "Year2", 
       	     omit.stat = c("ser"), 
         model.numbers = FALSE, 
             star.char = c("", "", ""), 
     omit.table.layout = "n")

print(" \n \n \n \n Average Hospital Charge")
stargazer(charge_ols, charge_re, charge_fe, 
                    se = list(coef(summary(charge_ols, cluster = c("State")))[,2], 
    	                      coef(summary(charge_re,  cluster = c("State")))[,2], 
       	                      coef(summary(charge_fe,  cluster = c("State")))[,2]), 
                 title = "Average Hospital Charge", 
        dep.var.labels = c("Charges"),
         column.labels = c("Pooled OLS" , "Random Effects", "Fixed Effects"), 
      covariate.labels = c("Units per 1000 Medicaid",
       	                   "Unemployment Rate",
       	                   "Legalization"),
       	        digits = 2, 
       	     add.lines = list(c("Year Effects", "Yes", "Yes", "Yes"), 
       	           	          c("Standard Errors", 
       	     	               "Clustered", "Clustered", "Clustered")), 
       	      no.space = TRUE, 
       	          omit = "Year2", 
       	     omit.stat = c("ser"), 
         model.numbers = FALSE, 
             star.char = c("", "", ""), 
     omit.table.layout = "n")

stop()
## -----------------------------------------------------------------------------
## Stargazer by model
## -----------------------------------------------------------------------------

print(" \n \n \n \n OLS Models")
stargazer(discharge_ols, LOS_ols, charge_ols, 
                    se = list(coef(summary(discharge_ols, cluster = c("State")))[,2], 
    	                      coef(summary(LOS_ols, cluster = c("State")))[,2], 
       	                      coef(summary(charge_ols, cluster = c("State")))[,2]), 
                 title = "Pooled OLS", 
         column.labels = c("Discharges" , "Avg. LOS", "Avg. Charge"), 
      covariate.labels = c("Units of LAI Reimbursed", 
       	                   "Units per 1000 Medicaid", 
       	                   "Unemployment Rate",
       	                   "Legalization"),
       	        digits = 2, 
       	     add.lines = list(c("Year Effects", "Yes", "Yes", "Yes"), 
       	           	          c("Standard Errors", 
       	     	               "Clustered", "Clustered", "Clustered")), 
       	      no.space = TRUE, 
       	          omit = "Year2", 
       	     omit.stat = c("f", "ser"), 
dep.var.labels.include = FALSE, 
         model.numbers = FALSE, 
             star.char = c("", "", ""), 
     omit.table.layout = "n")

print(" \n \n \n \n RE Models")
stargazer(discharge_re, LOS_re, charge_re, 
                    se = list(coef(summary(discharge_re, cluster = c("State")))[,2], 
    	                      coef(summary(LOS_re, cluster = c("State")))[,2], 
       	                      coef(summary(charge_re, cluster = c("State")))[,2]), 
                 title = "Random Effects", 
         column.labels = c("Discharges" , "Avg. LOS", "Avg. Charge"), 
      covariate.labels = c("Units of LAI Reimbursed", 
       	                   "Units per 1000 Medicaid", 
       	                   "Unemployment Rate",
       	                   "Legalization"),
       	        digits = 2, 
       	     add.lines = list(c("Year Effects", "Yes", "Yes", "Yes"), 
       	           	          c("Standard Errors", 
       	     	               "Clustered", "Clustered", "Clustered")), 
       	      no.space = TRUE, 
       	          omit = "Year2", 
       	     omit.stat = c("f", "ser"), 
dep.var.labels.include = FALSE, 
         model.numbers = FALSE, 
             star.char = c("", "", ""), 
     omit.table.layout = "n")

print(" \n \n \n \n FE Models")
stargazer(discharge_fe, LOS_fe, charge_fe, 
                    se = list(coef(summary(discharge_fe, cluster = c("State")))[,2], 
    	                      coef(summary(LOS_fe, cluster = c("State")))[,2], 
       	                      coef(summary(charge_fe, cluster = c("State")))[,2]), 
                 title = "Fixed Effects", 
         column.labels = c("Discharges" , "Avg. LOS", "Avg. Charge"), 
      covariate.labels = c("Units of LAI Reimbursed", 
       	                   "Units per 1000 Medicaid", 
       	                   "Unemployment Rate",
       	                   "Legalization"),
       	        digits = 2, 
       	     add.lines = list(c("Year Effects", "Yes", "Yes", "Yes"), 
       	           	          c("Standard Errors", 
       	     	               "Clustered", "Clustered", "Clustered")), 
       	      no.space = TRUE, 
       	          omit = "Year2", 
       	     omit.stat = c("f", "ser"), 
dep.var.labels.include = FALSE, 
         model.numbers = FALSE, 
             star.char = c("", "", ""), 
     omit.table.layout = "n")

## -----------------------------------------------------------------------------
