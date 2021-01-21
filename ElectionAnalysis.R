#### Packages ####

install.packages('ipumsr')
install.packages("usmap")
install.packages('papeR')
library(usmap)
library(ipumsr)
library(tidyverse)
library(readxl)
library(AER)
library(outreg)
library(papeR)
library(gridExtra)
library(ggimage)
setwd("/Users/johannes/Desktop/Minerva/Impact Evaluation")


#### DAG ####

library(ggdag)
theme_set(theme_dag())
dagified <- dagify(Biden ~ OpinionClimateChange + Income + PopDensity + Edu + Unemployment,
                   OpinionClimateChange ~ Income + PopDensity + Edu + Unemployment + HazardRisk, 
                   Income ~ HazardRisk,
                   Unemployment ~ HazardRisk,
                   PopDensity ~ Income + Unemployment + HazardRisk, 
                   Edu ~ HazardRisk,
                   exposure = "OpinionClimateChange",
                   outcome = "Biden",
                   labels = c("HazardRisk" = "Risk of Natural Disaster", 
                              "Biden" = "Margin of J. Biden\n in Presidential Election",
                              "OpinionClimateChange" = "Worried about climate change",
                              "PopDensity" = "County population density",
                              "Income" = "Median HH Income",
                              "Edu" = "Education",
                              "Unemployment" = "Unemployment \n Rate"
                              ))


ggdag(dagified, text = FALSE, use_labels = "label") 


#### DATA ####


#from https://github.com/tonmcg/US_County_Level_Election_Results_08-20/blob/master/2020_US_County_Level_Presidential_Results.csv
election_data <- read.csv("2020_US_County_Level_Presidential_Results.csv")

#https://hazards.geoplatform.gov/portal/apps/MapSeries/index.html?appid=ddf915a24fb24dc8863eed96bc3345f8
natural_hazard <- read.csv("NRI_Table_Counties.csv")

unemployment_us <- read_xls("Unemployment (1).xls")
population_us <- read_xls("PopulationEstimates.xls")
education_us <- read_xls("Education (1).xls")
poverty_us <- read_xls("PovertyEstimates (1).xls")

merged_covariates <- merge(unemployment_us, population_us, by = c('FIPStxt'))
merged_covariates <- merge(merged_covariates, education_us, by.x ='FIPStxt', by.y = "FIPS Code")
merged_covariates <- merge(merged_covariates, poverty_us, by ='FIPStxt')


climate_opinion <- read.csv("YCOM_2020_Data.csv")
merged_covariates$FIPSnum <- as.numeric(merged_covariates$FIPStxt)

merged_independent <- merge(merged_covariates, climate_opinion, by.x = "FIPSnum", by.y = 'GEOID')

merged <- merge(election_data, coast_counties, by.x = 'county_fips', by.y = 'STATE..COUNTY.FIPS', all.x = TRUE, all.y = FALSE)
merged[which(is.na(merged$isCoast)),"isCoast"] <- 0
merged$V.2016.POPULATION.ESTIMATE <- as.numeric(merged$V.2016.POPULATION.ESTIMATE)
merged[which(is.na(merged$V.2016.POPULATION.ESTIMATE)), "V.2016.POPULATION.ESTIMATE"] <- 0
merged$state_name <- as.factor(merged$state_name)

merged <- merge(merged, natural_hazard, by.x = 'county_fips', by.y = 'STCOFIPS', all.x = TRUE)
merged$STATE.FIPS <- as.factor(merged$STATE.FIPS)

merged_full <- merge(merged, merged_independent, by.x = 'county_fips', by.y = 'FIPSnum')
merged_full$density <- merged_full$POP_ESTIMATE_2019/merged_full$AREA

merged_full[which(merged_full$per_point_diff < 0),"BidenWon"] <- 1
merged_full[which(merged_full$per_point_diff > 0), "BidenWon"] <- 0
merged_full$BidenWon <- as.factor(merged_full$BidenWon)

merged_full$MinBachelor <-  merged_full$`Bachelor's degree or higher, 2014-18`/ merged_full$POP_ESTIMATE_2019

merged_full[merged_full$BidenWon == 1,"Pic"] <- "/Users/johannes/Desktop/Minerva/Impact Evaluation/miniBiden.png"
merged_full[merged_full$BidenWon == 0, "Pic"] <- "/Users/johannes/Desktop/Minerva/Impact Evaluation/miniTrump.png"

#### US MAP ####

library(usmap)

df_fips_vote <- merged_full[c('county_fips','per_point_diff')]
colnames(df_fips_vote) <- c('fips', 'per_point_diff')

plot_usmap(data = df_fips_vote, value = 'per_point_diff', exclude = c("Alaska")) +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red', name = "Difference in \nPercentage of Votes received")


df_fips_risk <- merged_full[c('county_fips','RISK_SCORE')]
colnames(df_fips_risk) <- c('fips', 'risk')

plot_usmap(data = df_fips_risk, value = 'risk', exclude = c("Alaska")) +
  scale_fill_viridis_c(option = "inferno", name = "Risk of \nNatural Hazard")


#### Descriptive Stats #### 


summary_table <- summarize(data =merged_full, 
          variables = c('per_point_diff', 
                        'happening',
                        'worried',
                        'president',
                        'RISK_SCORE', 
                        'density', 
                        'Median_Household_Income_2018',
                        'Unemployment_rate_2019' ,
                        "MinBachelor"),
          group = "BidenWon", weigths = "total_votes")

if (require("xtable")) {
  xtable(summary_table)
} 
print.xtable(summary_table)


theme_set(theme_classic())

hist_outcome_unweighted <- ggplot(data = merged_full, aes(x = per_point_diff)) + 
  geom_histogram() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) + 
  labs(caption = "Panel A", x = "Vote Margin", y = '') 

hist_outcome_weighted <- ggplot(data = merged_full, aes(x = per_point_diff, weights = total_votes)) + 
  geom_histogram() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) + 
  labs(caption = "Panel B",x = "Vote Margin (weighted)", y = '') 

grid.arrange(hist_outcome_unweighted, hist_outcome_weighted, nrow = 1) 

raw_correlation_plot <- ggplot(data = merged_full, aes(x = happening, y = per_point_diff)) +
  geom_point(aes(size = total_votes)) + 
  labs(x = "Agree climate change is happening (%)", y = "Vote Margin (%-point)") 
raw_correlation_plot + 
  scale_radius(name="Total Votes \nof County")


## same plot but make every marker the face of Donald Trump or Joe Biden
correlation_plot_faces <- ggplot(data = merged_full, aes(x = happening, y = per_point_diff)) +
  geom_point() + 
  geom_image(aes(image = Pic, size = I(log(total_votes*100))/1050)) + #select/unselect this to get just the geom_points/images Donald and Joe.
  labs(x = "Agree climate change is happening (%)", y = "Vote Margin (%-point)")
correlation_plot_faces

summary(lm(happening ~ RISK_SCORE, data = merged_full))

#### Model ####

#first stage estimation 
first_stage_reg <- lm(happening ~  +RISK_SCORE + density + Median_Household_Income_2018  + Unemployment_rate_2019 + `Bachelor's degree or higher, 2014-18`+I(state_name),
                               data = merged_full, 
                               weights = total_votes)
first_stage <- outreg(first_stage_reg, constlast = T)

#first stage without fixed effects
first_stage_reg_nfe <- lm(happening ~  +RISK_SCORE + density + Median_Household_Income_2018  + Unemployment_rate_2019 + `Bachelor's degree or higher, 2014-18`,
                      data = merged_full, 
                      weights = total_votes)
first_stage_nfe <- outreg(first_stage_reg_nfe, constlast = T)


#full IV with fixed effects
IV_reg <- ivreg(per_point_diff ~ happening + density + Median_Household_Income_2018  + Unemployment_rate_2019 + `Bachelor's degree or higher, 2014-18`+I(state_name) |
            RISK_SCORE + density + Median_Household_Income_2018  + Unemployment_rate_2019 + `Bachelor's degree or higher, 2014-18`+I(state_name),
             data = merged_full, 
            weights = total_votes)

IV <- outreg(IV_reg, constlast = T)


#full IV without fixed effects
IV_reg_nfe <- ivreg(per_point_diff ~ happening + density + Median_Household_Income_2018  + Unemployment_rate_2019 + `Bachelor's degree or higher, 2014-18` |
                  RISK_SCORE + density + Median_Household_Income_2018  + Unemployment_rate_2019 + `Bachelor's degree or higher, 2014-18`,
                data = merged_full, 
                weights = total_votes)

IV_nfe  <- outreg(IV_reg_nfe , constlast = T)

