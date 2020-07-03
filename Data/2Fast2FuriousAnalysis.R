# 2Fast2Furious
###### Hackathon 03/07/2020

rm(list = ls())

#Load libraries
library(dplyr)
library(ggplot2)

# get the data
setwd("/Users/Hebe/Documents/PhD_ExeterUniversity/Exeter_PhD/TrainingExperience/Hackathon2020Covid19")
Pop <- read.csv("UKpopulationEstimate.csv")
Depri <- read.csv("IMDdeprivationScore.csv")
Green <- read.csv("regionGreenSpace.csv")
covid <- read.csv("cases_latest.csv")
covid$Specimen.date <- as.Date(covid$Specimen.date)

# filter the dates
covid <- filter(covid, Specimen.date > "2020-02-29" & Specimen.date <"2020-06-01")

######### Wrangle data ------------------
# choose a specific date
covid <- covid %>% filter(Area.type == "Upper tier local authority")
#alldata <- Pop[(Pop$name %in% Green$name),]

# join data
all <- Pop %>% right_join(Green)
all <- all %>% right_join(Depri)
all <- all %>% right_join(covid)

# remove rows that don't match/ have NA
all <-na.omit(all)

# rename columns
all <-rename(all, pop.dens = Allages)

# remove columns that aren't required
all1 <- all %>% select(-Code, -Geography1, -LocalAuthoritycode, -Area.code, -Area.type, -CumulativeCases)


# -----------------------------------------------------------------------------------
############# check the corrolations over time - does this change with lockdown? ----------

# add the pearson correation coeffeicient and p-value to the dataframe
cor <- all1 %>% group_by(Specimen.date) %>% mutate(cor_coef = cor.test(CumulativeRate, totGreenSpaces_sqm, method = "pearson")$estimate,
                   p_val = cor.test(CumulativeRate, totGreenSpaces_sqm, method = "pearson")$p.value)

# plot the data
cor_plot <- ggplot(cor, aes(Specimen.date, cor_coef)) + 
  geom_point(col = "navy") + 
  xlab("Date") + ylab("Correlation coeffiecient") +
  theme_bw()

p_plot <- ggplot(cor, aes(Specimen.date, p_val)) + geom_point(col = "darkorange") +
  xlab("Date") + ylab("p-value") + 
  geom_hline(yintercept = 0.05, linetype = "dashed", colour = "brown") + theme_bw()


ggarrange(cor_plot, p_plot, nrow = 2)






#------------------------------------------------------------------------------------------------
########## Linear models ##########################

all <- read.csv("MergedData.csv", header = T)

all$Specimen.date <- as.Date(all$Specimen.date)
all <- all %>% filter(Specimen.date == "2020-04-01")

head(all)


### examine normality ###

hist(all$pop.dens) #normal
hist(all$totGreenSpaces_sqm) #this looks a little skewed
hist(log(all$totGreenSpaces_sqm)) 
hist(all$IMD) #normal
hist(all$CumulativeRate) 
hist(log(all$CumulativeRate))  

### linear modelling time ### 

lm1 <- lm(log(all$CumulativeRate) ~ all$totGreenSpaces_sqm + all$pop.dens + all$IMD)
summary(lm1)
lm2<- lm(log(all$CumulativeRate) ~ all$pop.dens + all$IMD)
summary(lm2)
anova(lm2,lm1)

# most variance explained in data when Green space is included. Less explanitory power of IMD and pop density without green space. 

### check the residuals ### 

plot(lm1)

### plot that puppy ###

ggplot(all, aes(x=totGreenSpaces_sqm, y=CumulativeRate)) +
  geom_point(shape=1) +    
  geom_smooth(method = lm, color = "green") + 
  theme_classic()+ 
  xlab("Total Green Spaces (Square Meters)") + ylab("Rate of Infection per 100000")





