#these libraries need to be loaded
library(utils)
library(dplyr)
library(ggplot2)
library(ggrepel)
## library(hrbrthemes)
library(viridis)

nbDays <- 270
nbCases <- 1000

#read the Dataset sheet into “R”. The dataset will be called "data".
data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM")

## groupby country and sort by date
data <- data %>% mutate(dateRep = as.Date(dateRep, "%d/%m/%Y")) %>% arrange(countriesAndTerritories, dateRep)

## Adding & filling the label column
indexMaxDate = data %>% group_by(countriesAndTerritories) %>% summarise(maxDateIdx = which.max(as.Date(dateRep))) %>% mutate(label = cumsum(maxDateIdx))
data$label<-rep(NA_character_, nrow(data))
data$label[indexMaxDate$label]<-indexMaxDate$countriesAndTerritories

countries<-c("Lebanon", "Australia", "China", "United_States_of_America","United_Kingdom","Italy","Spain","Iran","France","South_Korea","Japan","Germany","Singapore", "Brazil", "India", "Mexico", "Belgium", "Serbia", "Croatia", "Austria", "Slovakia", "Jordan")

## keep desired countries
df <- data %>% filter(countriesAndTerritories %in% countries)

## get cumulative sums
df <- df %>% group_by(countriesAndTerritories) %>% mutate(cumsum = cumsum(cases))

m <- subset(df, cumsum > nbCases) 

# Add days Since nbCases
m <- m %>% group_by(countriesAndTerritories) %>%  mutate(daysSinceNbCases = 1:length(countriesAndTerritories))

## plotting reported cases per days since nbCases reached
p <-ggplot(m, aes(daysSinceNbCases, log10(cumsum), group=factor(countriesAndTerritories), color=factor(countriesAndTerritories))) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_fill_brewer(palette="Paired") +
  geom_label_repel(aes(label = label), nudge_x = 1, na.rm = TRUE) +
  labs( title= paste("COVID-19 Case Growth after ", nbCases, " cases (", Sys.Date(), ")", sep = ""), y="Total reported cases (log10)", x = paste("Days since", nbCases,"cases", sep = " "))

mynamestheme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
                      legend.title = element_text(face = "bold.italic", family = "Helvetica"), 
                      legend.text = element_text(face = "italic", family = "Helvetica"), 
                      axis.title = element_text(family = "Helvetica", face = "bold", size = (12)),
                      axis.text = element_text(family = "Courier", face = "bold", size = (12)),
                      legend.position = "none")

p + mynamestheme

pdf("~/project/COVID_19_Plotting/covid_19_case_growth_ecdc.pdf", width = 12)
p + mynamestheme
dev.off()

countries_sel <- c("Lebanon", "Australia", "China", "United_States_of_America","United_Kingdom","Italy","Spain","France","South_Korea","Germany","Singapore", "Croatia", "Austria", "Slovakia", "Jordan")
## plotting 
p <-ggplot(df %>% subset(., countriesAndTerritories %in% countries_sel), 
           aes(dateRep, Cumulative_number_for_14_days_of_COVID.19_cases_per_100000,
               group=factor(countriesAndTerritories),
               color=factor(countriesAndTerritories))) +
  geom_smooth(method = "loess", se = TRUE, span = 0.1) +
  geom_label_repel(aes(label = label), nudge_x = 2, na.rm = TRUE) +
  xlim(as.Date(c("2020-03-01","2020-10-31"))) +
  scale_fill_brewer(palette="Paired") +
  labs( title= paste("Cumulative Numbers for 14 days of COVID19 cases per 100,000 (", Sys.Date(), ")", sep = ""), y="Cumulative Count in the last 14 days", x = paste("Date", sep = " "))

mynamestheme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
                      legend.title = element_text(face = "bold.italic", family = "Helvetica"), 
                      legend.text = element_text(face = "italic", family = "Helvetica"), 
                      axis.title = element_text(family = "Helvetica", face = "bold", size = (12)),
                      axis.text = element_text(family = "Courier", face = "bold", size = (12)),
                      legend.position = "none")
pdf("~/project/COVID_19_Plotting/covid_19_cumulative_14_days_100000.pdf", width = 12)
p + mynamestheme
dev.off()

## plotting Nb cases/pop size
df %>% 
  group_by(countriesAndTerritories) %>%
  summarize(cases = max(cumsum), pop = max(popData2019)) %>%
  ggplot(aes(x = log10(pop), y = cases/pop*1000000, fill=factor(countriesAndTerritories))) +
  geom_point(alpha = 0.5, shape=21, color="black", size = 3) +
  labs( title= paste("Total cases per population size (Scaled for 1M; ", Sys.Date(), ")", sep = ""), y="Total Cases per 1M pop", x = paste("Population size (log10)", sep = " ")) +
  geom_label_repel(aes(label = countriesAndTerritories),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  scale_fill_viridis(alpha = 0.2, discrete=TRUE, guide=FALSE, option="B") + 
  ylim(c(0, 25000))


## looking only at countries with comparable pop size
data %>% 
  filter(popData2019 > 5000000 & popData2019 < 8000000) %>% group_by(countriesAndTerritories) %>%
  ggplot(aes(dateRep, Cumulative_number_for_14_days_of_COVID.19_cases_per_100000,
             group=factor(countriesAndTerritories),
             color=factor(countriesAndTerritories))) +
  geom_smooth(method = "loess", se=FALSE, span = 0.1) +
  geom_label_repel(aes(label = label), nudge_x = 2, na.rm = TRUE) +
  scale_fill_brewer(palette="Paired") +
  labs( title= paste("Cumulative Numbers for 14 days of COVID19 cases per 100,000 (", Sys.Date(), ")", sep = ""), y="Cumulative Numbers for 14 days per 100,000 persons", x = paste("Date", sep = " ")) +
  xlim(as.Date(c("2020-03-01","2020-10-31"))) +
  theme(legend.position = "none")

  


