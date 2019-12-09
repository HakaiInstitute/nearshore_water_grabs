# --- Hakai Surface Water Grabs (CHLa) data loading and formatting ---
## Author:       Lauran Liggan ##
## Date Created: 2019-01-28    ##
## Notes:      : Part of main summary script and
##               separated to make code more manageable and modular.

# This script contains code for loading and [some simple] tidying of the
#   Hakai Surface Water Grabs data subsets, specificallty CHLa, as well as any necessary packages. 

#
#   The following script will load the above dataset, as well as do some minor
#   formatting (ie. set variable classes, create new variables, rename
#   variables, etc.).


# --- Packages ---
# The following code installs and loads all the necessary packages. It is 
#   commented out by default. Be sure to check you have all these packages
#   installed, remove the comment and run if necessary, otherwise the majority
#   of summaries and figures will not work.

## Install packages ----
install.packages(c("plyr", "dplyr", "ggplot2", "magrittr", "lubridate", "knitr", "reshape2", "gridExtra", "googlesheets", "plotly", "tidyr"))


## Load packages ----
# Load packages installed above

lapply(c("dplyr", "ggplot2", "magrittr", "reshape2", "lubridate"),
       library, character.only = T)

library(plotly)
library(googlesheets)
library(ggthemes)
library(ggplot2)
library (Rmisc)

# --- Packages end ----

# --- Load data ---
#   The following code loads the CHLa datasets, rename some variables,
#   and create some new variables.

## Set a loading directory ----
# Save all csv files in CHLa -> 2018 -> Data folder on Dropbox

setwd("~/Desktop/GitHub projects/nearshore_water_grabs/CHLa") 

getwd()

## Load DO13C data ----

LL2018_CHLaData <- read.csv(file="CHLa_2017_2018.csv",
                             header           = TRUE,
                             stringsAsFactors = FALSE,
                             colClasses  = c("date"  = "character")) %>%
  
  mutate(date      = ymd(gsub("T{1}.+", "", date)),
         monthNum         = factor(gsub("(.{4}-)|(-.{2})", "", date)),
         month            = as.factor(ifelse(monthNum == "05", "May",
                                      ifelse(monthNum == "06", "June",
                                      ifelse(monthNum == "08", "August", 
                                      ifelse(monthNum == "04", "April",
                                      ifelse(monthNum == "02", "February",
                                      ifelse(monthNum == "03", "March",
                                      ifelse(monthNum == "09", "September",
                                      ifelse(monthNum == "07", "July",
                                      ifelse(monthNum == "09", "September",
                                      ifelse(monthNum == "10", "October",
                                      ifelse(monthNum == "11", "November",
                                      ifelse(monthNum == "12", "December",
                                      ifelse(monthNum == "01", "January",NA))))))))))))))) %>%
  
  mutate(Site             = as.factor(ifelse(site == "ROCKY02", "Foggy Cove",
                                      ifelse(site == "ROCKY04", "Little Wolf", 
                                      ifelse(site == "ROCKY05", "Nalau",
                                      ifelse(site == "ROCKY06", "West Beach",
                                      ifelse(site == "ROCKY07", "North Beach",
                                      ifelse(site == "ROCKY03", "5th Beach",NA))))))))

glimpse(LL2018_CHLaData)

# --- Load data end ---

# --- Clean data ---

## Clean DO13C dataset ----

CHla2018 <- na.omit(LL2018_CHLaData)
NROW(CHla2018)

# recode late 2017 June months to "07"

CHla2018$monthNum <- as.character(CHla2018$monthNum )

CHla2018$monthNum[ CHla2018$date > ymd( "2017-06-15" ) & 
                     CHla2018$date < ymd( "2017-07-01" ) ] <- "07"

CHla2018$monthNum <- factor(CHla2018$monthNum )

CHla2018$month <- as.character( CHla2018$month)
CHla2018$month[ CHla2018$date > ymd( "2017-06-15" ) & 
                  CHla2018$date < ymd( "2017-07-01" ) ] <- "July"

# Factor month in proper order
CHla2018$month <- factor(CHla2018$month)

CHla2018$month <- factor(CHla2018$month, levels = c("Januray", "Feburary", "March", "April","May", "June", "July", "August", "September", "October", "November", "December"))

# Create a year column
CHla2018$year<- year(CHla2018$date)

# View data
glimpse(CHla2018)

# --- Clean data end ---

# --- Summarize dataset ---
## Summarize DO13C dataset ----

CHlaSum2018<- summarySE(CHla2018,
                         measurevar="Chla", 
                         groupvars=c("month", "year", "Site", "Plankton.Size", 
                                     na.rm=FALSE))
CHlaSum2018

PhaeoSum2018<- summarySE(CHla2018,
                        measurevar="Phaeo", 
                        groupvars=c("month", "year", "Site", "Plankton.Size", 
                                    na.rm=FALSE))
PhaeoSum2018


CHLa.Micro<- filter(CHla2018, Plankton.Size == "Micro", year == "2018")

# --- Summarize dataset end ---

# --- Summary figures ---
## DO13C summary figures ---- 

# format aesthetics

theme_set(theme_classic(base_size = 16))

pd <- 0.5

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# by site
# trends between sites

CHlaSum2018$Plankton <- CHlaSum2018$Plankton.Size
CHLaFigSites <- ggplot(data=subset(CHlaSum2018, year == "2018"), aes(x=month, 
                                              y=Chla, 
                                              col= Plankton,
                                              ymin=Chla-se,
                                              ymax=Chla+se)) +
  geom_errorbar( position=position_dodge(pd), width=.5) +
  ylab("Chlorophyll (microg/L)") +
  xlab("Month")+ 
  geom_point( position=position_dodge(pd), size=3) +
  theme(panel.grid.major = element_line(colour = "grey90", linetype = 3)) +
  geom_line( position=position_dodge(pd), aes(group=Plankton, linetype= Plankton)) +
  facet_wrap(~Site, ncol = 3)+
  theme(axis.text=element_text(size=12))+
  scale_color_manual(values = cbPalette) +
  theme(axis.text.x=element_text(hjust=1)) +
  theme(legend.justification=c(0,1), legend.position=c(0.01,0.99), legend.text = element_text(size = 12)) +
  scale_x_discrete(limits = c("Feburary", "March", "April", "May","June", "July", "August"), 
                   breaks = c("Feburary", "March", "April", "May","June", "July", "August")) +
  ggtitle(~italic("Nearshore Surface Water"), "CHLa 2018") +
  theme(axis.text.x=element_text(hjust=1, angle=45))  

CHLaFigSites

# Print DO13C
CHLaFig_print <- ggsave(plot = CHLaFigSites, width = 8, height = 9, dpi = 600, filename = "CHLa_2018_by site.png")



#dot plot of all sites
CHLaPlot <- ggplot(data=subset(CHla2018, year == "2018"), aes(x=month, 
                                     y=Chla, col=Site)) +
  geom_point(shape = 16, size = 3, position=position_dodge(pd)) +
  scale_fill_manual(values = cbPalette) +
  scale_color_manual(values = cbPalette) +
  ggtitle(~italic("Nearshore Surface Water"), "CHLa 2018") +
  ylab("Chlorophyll (microg/L)") +
  xlab("Month") +
  theme(panel.grid.major = element_line(colour = "grey90", linetype = 3)) +
  theme(axis.text.x=element_text(hjust=1)) +
  theme(legend.justification=c(0,1), legend.position=c(0.76,1.1), legend.text = element_text(size = 12), legend.background = element_rect(linetype = 2, size = 0.5, colour = 1)) +
  theme(plot.title = element_text(hjust = -0.01, vjust=1.12, size =20)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

CHLaPlot

# Print 
CHla2018Fig_print <- ggsave(plot =  CHLaPlot, width = 8, height = 6, dpi = 600, filename = "CHLaPlot_2018_all sites.png")

# trends accross all sites

CHla2018$Plankton <- CHla2018$Plankton.Size
CHla2018Fig <- ggplot(data=subset(CHla2018, year == "2018"), aes(month, Chla, col=Plankton)) +
  geom_boxplot() + 
  theme(panel.grid.major = element_line(colour = "grey90", linetype = 3)) +
  scale_color_manual(values = cbPalette) + 
  ylab("Chlorophyll (microg/L)") +
  xlab("Month")+ 
  theme(axis.text.x=element_text(hjust=1)) +
  theme(axis.text.y=element_text(hjust=1)) +
  ggtitle(~italic("Nearshore Surface Water"), "CHLa 2018") + 
  theme(plot.title = element_text(hjust = -0.01, vjust=1.12, size =20)) +
  theme(legend.justification=c(0,0.5), legend.position=c(0.8,1),legend.text = element_text(size = 12)) +
  theme(axis.text=element_text(size=12))+
  theme(axis.text.x=element_text(hjust=1, angle=45))

CHla2018Fig

# Print 
CHla2018Fig_print <- ggsave(plot = CHla2018Fig, width = 6, height = 6, dpi = 600, filename = "CHLa_2018_all sites.png")



# --- Summary figures end ---