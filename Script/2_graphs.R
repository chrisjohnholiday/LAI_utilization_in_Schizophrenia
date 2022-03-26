## -----------------------------------------------------------------------------
#'  2_graphs.R
#'
#' by: Chris Holiday
#'
#'  Creates graphs for exploratory data analysis
#'
#'
## -----------------------------------------------------------------------------
## Managing dependencies
## -----------------------------------------------------------------------------

library(data.table)
library(ggplot2)
library(usmap)
library(extrafont)

## Import data ##
lai_data <-fread(file = "/home/choliday/ECO 2408/LAI_Utilization/Output/lai_data_2020-11-26.csv", sep = ",")

## -----------------------------------------------------------------------------
## Import and Clean population data
## -----------------------------------------------------------------------------

pop_raw <-fread(file.path(raw, "US_census_2000_2019.csv"), 
	sep = ",", header = TRUE)

## Convert state column ##
pop_raw$State <- gsub("[.]", "", pop_raw$State)
pop_raw$State <- state.abb[match(pop_raw$State, state.name)]

## Reshape table ##
pop_long <- melt(pop_raw, id = c("State"))
colnames(pop_long) <- c("State", "Year", "Population")

# year column causing merge issues
pop_long$Year <- as.numeric(as.character(pop_long$Year))

## Merge to HCUP ##
lai_data <- merge(x = lai_data, y = pop_long, by = c("State", "Year"), 
	              all.x= TRUE)
lai_data$discharge_cap <- as.numeric(lai_data$discharge) / as.numeric(lai_data$Population) * 1000


## -----------------------------------------------------------------------------
## Binnning for Graphs
## -----------------------------------------------------------------------------

## Summary statistics of LAI units dispensed##
lai_usage <- lai_data[Year > 2010,]
lai_usage_distribution <- aggregate(cbind(scripts_cap, LAI_cap) ~ State, data = lai_usage[Year > 2013,], FUN=mean)

## Define high utilization states ##
high_util <- c("MO", "NY", "NC", "FL", "NJ", "WI", "NE", "TN")
mid_util  <- c("UT", "IA", "KS", "CA", "WV", "MN", "HI", "MI")
low_util  <- c("SC", "MA", "AZ", "WA", "CO", "VT", "OR", "KY")

## Alternative definition ##
# high_util <- c("MO", "NY", "NC", "NJ")
# mid_util  <- c("FL", "WI", "NE", "IA", "TN", "KS", "UT")
# low_util  <- c("CA", "WV", "HI", "MI", "MN", "AZ", "SC", "MA", "WA", "CO", "VT", "OR", "KY")


## Define groups ##
lai_usage$Group <- "high_util"
lai_usage$Group[lai_usage$State %in% mid_util] <- "mid_util"
lai_usage$Group[lai_usage$State %in% low_util] <- "low_util"
lai_data$Group <- "high_util"
lai_data$Group[lai_data$State %in% mid_util] <- "mid_util"
lai_data$Group[lai_data$State %in% low_util] <- "low_util"

## Group by groups ##
lai_means <- aggregate(cbind(LAI_cap) ~ Group + Year, data = lai_usage, FUN=mean)
discharge_means <- aggregate(cbind(discharge_cap) ~ Group + Year, data = lai_data, FUN=mean)
los_charge_means <- aggregate(cbind(LOS, charge) ~ Group + Year, data = lai_data, FUN=mean)


## -----------------------------------------------------------------------------
## Map
## -----------------------------------------------------------------------------

## Map Continous ##
map_data <- lai_usage_distribution[, c("State", "LAI_cap")]
colnames(map_data) <- c("state", "LAI_cap")


lai_map1 <- plot_usmap(data = map_data, 
                     values = "LAI_cap") + 
 scale_fill_continuous(name = "LAI Units Per 1000 Medicaid", 
                      label = scales::comma) +
ggtitle("Average LAI Units Reimbursed Per 1000 Medicaid Enrollees (2013-2017)") +
      theme(legend.position = "right")

ggsave(file = file.path(output, "lai_map_continous.png"), 
       plot = lai_map1)

## Map Discrete ##
lai_usage_distribution$Group <- "high_util"
lai_usage_distribution$Group[lai_usage_distribution$State %in% mid_util] <- "mid_util"
lai_usage_distribution$Group[lai_usage_distribution$State %in% low_util] <- "low_util"
lai_usage_distribution$Group <- as.factor(lai_usage_distribution$Group)
colnames(lai_usage_distribution)[1] <- c("state")

lai_map2 <- plot_usmap(data = lai_usage_distribution,
                     values = "Group") + 
     scale_fill_manual(name = "Utilization Level",
                     labels = c("High", "Low", "Mid"),
                     values = c("firebrick2",  "chartreuse3",   "royalblue3")) +
ggtitle("Levels of LAI Utilization") +
      theme(legend.position = "right")

ggsave(file = file.path(output, "lai_map_discrete.png"), 
       plot = lai_map2)

##---------------------------------------------------------------------
## Trend Graphs
## -----------------------------------------------------------------------------


## LAI graph ##
lai_means_plot <- ggplot(
	    data = lai_means,
       aes(x = Year, 
       	   y = LAI_cap, 
      colour = Group)) +
       geom_line(size = 1.5) + 
       scale_x_continuous(expand = c(0, 0), limits=c(2011, 2017)) +
       scale_y_continuous(expand = c(0, 0), limits=c(0,max(lai_means$LAI_cap))) +
         scale_color_manual(name = "Utilization Level",
                          labels = c("High", "Low", "Medium"),
                          values = c("firebrick2",  "chartreuse3", "royalblue3")) +
       theme_classic() +
       theme(text = element_text(size=20)) +
       ggtitle("Average Units of LAI Reimbursed Per 1000 Medicaid Enrollees") + 
       xlab("Year") + 
       ylab("LAIs Reimbursed")

ggsave(file = file.path(output, "LAI_means.png"), 
	   plot = lai_means_plot, 
	  width = 14, 
	 height = 8,
	    dpi = 300)

## discharge graph ##
discharge_means_plot <- ggplot(
	    data = discharge_means,
       aes(x = Year, 
       	   y = discharge_cap, 
      colour = Group)) +
       geom_line(size = 1.5) + 
       geom_vline(xintercept= 2011, linetype = "dashed") +
       scale_x_continuous(expand = c(0, 0), limits=c(2001, 2017), breaks = discharge_means$Year) +
       scale_y_continuous(expand = c(0, 0), limits=c(0, max(discharge_means$discharge_cap))) +
         scale_color_manual(name = "Utilization Level",
                          labels = c("High", "Low", "Medium"),
                          values = c("firebrick2",  "chartreuse3", "royalblue3")) +
       theme_classic() +
       theme(text = element_text(size=20)) +
       ggtitle("Average Schizophrenia Discharges Per Capita") + 
       xlab("Year") + 
       ylab("Discharges Per Capita") +
       labs(caption = "Dashed line indicates introduction of LAI's. \n 2015 year missing due to change in billing code system.")

ggsave(file = file.path(output, "discharge_means.png"), 
	   plot = discharge_means_plot, 
	  width = 14, 
	 height = 8, 
	    dpi = 300)


## LOS graph ##
los_means_plot <- ggplot(
	    data = los_charge_means,
       aes(x = Year, 
       	   y = LOS, 
      colour = Group)) +
       geom_line(size = 1.5) + 
       geom_vline(xintercept= 2011, linetype = "dashed") +
      scale_x_continuous(expand = c(0, 0), limits=c(2001, 2017)) +
       scale_y_continuous(expand = c(0, 0), limits=c(7,max(los_charge_means$LOS))) +
         scale_color_manual(name = "Utilization Level",
                          labels = c("High", "Low", "Medium"),
                          values = c("firebrick2",  "chartreuse3",   "royalblue3")) +
       theme_classic() +
       theme(text = element_text(size=13)) +
       ggtitle("Average Length of Stay for Schizophrenia Inpatient") + 
       xlab("Year") + 
       ylab("Length of Stay (LOS)") +
       labs(caption = "Dashed line indicates introduction of LAI's.")


ggsave(file = file.path(output, "los_means.png"), plot = los_means_plot, 
    width = 10, 
   height = 6, 
      dpi = 300)

## charge graph ##
los_means_plot <- ggplot(
	    data = los_charge_means,
       aes(x = Year, 
       	   y = charge, 
      colour = Group)) +
       geom_line(size = 1.5) + 
       geom_vline(xintercept= 2011, linetype = "dashed") +
       scale_x_continuous(expand = c(0, 0), limits=c(2001, 2017)) +
       scale_y_continuous(expand = c(0, 0), limits=c(10000,max(los_charge_means$charge))) +
         scale_color_manual(name = "Utilization Level",
                          labels = c("High", "Low", "Medium"),
                          values = c("firebrick2",  "chartreuse3",   "royalblue3")) +
       theme_classic() +
       theme(text = element_text(size=13)) +
       ggtitle("Average Hospital Charge for Schizophrenia Inpatient") + 
       xlab("Year") + 
       ylab("Length of Hosital Charge ($USD 2017)") +
       labs(caption = "Dashed line indicates introduction of LAI's.")

ggsave(file = file.path(output, "charge_means.png"), plot = los_means_plot, 
    width = 10, 
   height = 6, 
      dpi = 300)


## -----------------------------------------------------------------------------

## Todo: 1. cost adjust hospital charges 2. add vertical lines of LAI introduction 2011
## 3. Increase font size in larger graphs
