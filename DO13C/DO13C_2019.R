# --- Hakai Surface Water Grabs (DO13C) data loading and formatting ---
## Author:       Lauran Liggan ##
## Date Created: 2018-10-22    ##
## Notes:      : Part of main summary script and
##               separated to make code more manageable and modular.

# This script contains code for loading and [some simple] tidying of the
#   Hakai Surface Water Grabs data subsets, specificallty DO13C, as well as any necessary packages. 

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
#   The following code loads the DO13C datasets, rename some variables,
#   and create some new variables.

## Set a loading directory ----
# Save all csv files in DO13C -> 2018 -> Data folder on Dropbox

setwd("~/Desktop/GitHub projects/nearshore_water_grabs/DO13C") 

getwd()


## Load ROCKY DO13C data ----

LL2019_do13cData <- read.csv(file="DO13C_2017_2018_2019.csv",
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
                                      ifelse(site == "ROCKY03", "5th Beach",
                                      ifelse(site == "ROCKY08", "Cape Calvert",
                                      ifelse(site == "ROCKY09", "Admiral Group",NA))))))))))  

glimpse(LL2019_do13cData)


## Load ALL DO13C data ----

do13c2019 <- read.csv(file="do13c_2019.csv",
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
                                      ifelse(site == "ROCKY03", "5th Beach",
                                      ifelse(site == "PRUTH", "Pruth Bay",
                                      ifelse(site == "QCS01", "Queen Charlotte Sound",NA))))))))))  

glimpse(do13c2019)


# --- Load data end ---

# --- Clean data ---

## Clean DO13C dataset ----

do13cROCKY <- na.omit(LL2019_do13cData)
NROW(do13cROCKY)

do13c2019 <- na.omit(do13c2019)
NROW(do13c2019)


# recode late 2017 June months to "07"

do13c2019$monthNum <- as.character(do13c2019$monthNum )

do13c2019$monthNum[ do13c2019$date > ymd( "2017-06-15" ) & 
                      do13c2019$date < ymd( "2017-07-01" ) ] <- "07"

do13c2019$monthNum <- factor(do13c2019$monthNum )

do13c2019$month <- as.character( do13c2019$month)
do13c2019$month[ do13c2019$date > ymd( "2017-06-15" ) & 
                   do13c2019$date < ymd( "2017-07-01" ) ] <- "July"

# Factor month in proper order
do13c2019$month <- factor(do13c2019$month)

do13c2019$month <- factor(do13c2019$month, levels = c("Januray", "February", "March", "April","May", "June", "July", "August", "September", "October", "November", "December"))


# Create a year column
do13c2019$year<- year(do13c2019$date)

# filter out data above 1m
# filter out data above 4 ppm (anomolies)
do13c20191m<- filter(do13c2019, line_out_depth < 1, ppm_c_doc < 4)


# View data
glimpse(do13c20191m)


# --- Clean data end ---

# --- Summarize dataset ---
## Summarize DO13C dataset ----

do13cSum2019<- summarySE(do13c20191m,
                     measurevar="ppm_c_doc", 
                     groupvars=c("month", "year", "Site",
                                 na.rm=FALSE))
do13cSum2019

# --- Summarize dataset end ---

# --- Summary figures ---
## DO13C summary figures ---- 

# format aesthetics

theme_set(theme_classic(base_size = 16))

pd <- 0.5

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# by site
#trends between years (keeping years continuous)
do13cFigSites <- ggplot(data=do13cSum2019, aes(x=month, 
                                      y=ppm_c_doc, 
                                      col= Site,
                                      ymin=ppm_c_doc-sd,
                                      ymax=ppm_c_doc+sd)) +
  geom_errorbar( position=position_dodge(pd), width=.5) +
  ylab("C13 (ppm)") +
  xlab("Month")+ 
  geom_point( position=position_dodge(pd), size=3) +
  theme(panel.grid.major = element_line(colour = "grey90", linetype = 3)) +
  geom_line( position=position_dodge(pd), aes(group=Site), linetype= 2) +
  facet_wrap(Site~as.factor(year), ncol = 3)+
  scale_color_manual(values = cbPalette) +
  theme(axis.text.x=element_text(hjust=1)) +
  theme(axis.text=element_text(size=12))+
  theme(plot.title = element_text(hjust = -0.01, vjust=1.12, size =20))  +
  ggtitle(~italic("Nearshore Surface Water"), "DO13C") +
  theme(legend.position="none")+
  theme(axis.text.x=element_text(hjust=1, angle=45))  

do13cFigSites

# by year
# trends between years accross all sites (year plotted separately for each site)
do13cFigYear <- ggplot(data=do13cSum2019, aes(x=month, 
                                          y=ppm_c_doc, 
                                          col= as.factor(year),
                                          ymin=ppm_c_doc-sd,
                                          ymax=ppm_c_doc+sd)) +
  geom_errorbar( position=position_dodge(pd), width=0) +
  ylab("C13 (ppm)") +
  xlab("Month")+ 
  geom_point( position=position_dodge(pd), size=3) +
  theme(panel.grid.major = element_line(colour = "grey90", linetype = 3)) +
  geom_line( position=position_dodge(pd), aes(group=as.factor(year), linetype= as.factor(year))) +
  facet_wrap(~Site, ncol = 2)+
  theme(axis.text=element_text(size=12))+
  scale_color_manual(values = cbPalette) +
  theme(axis.text.x=element_text(hjust=1)) +
  theme(legend.title=element_blank(), legend.justification=c(0,1), legend.position=c(0.85,0.99), legend.text = element_text(size = 12)) +
  ggtitle(~italic("Nearshore Surface Water"), "DO13C") +
  theme(axis.text.x=element_text(hjust=1, angle=45))  


do13cFigYear

# Print DO13C
do13cFig_print <- ggsave(plot = do13cFigYear, width = 7, height = 10, dpi = 600, filename = "DO13C_by site.png")


# by year
# trends between years accross all sites

do13cFig <- ggplot(do13c20191m, aes(month, ppm_c_doc)) +
  geom_boxplot(aes(fill=Site), size=0.5) +   

  theme(panel.grid.major = element_line(colour = "grey90", linetype = 3)) +
  scale_color_manual(values = cbPalette) + 
  ylab("C13 (ppm)") +
  xlab("Month") +
  facet_wrap(~as.factor(year), ncol = 1) +
  theme(axis.text.x=element_text(hjust=1)) +
  theme(axis.text.y=element_text(hjust=1)) +
  ggtitle(~italic("Nearshore Surface Water"), "DO13C") + 
  theme(plot.title = element_text(hjust = -0.01, vjust=1.12, size =20)) +
  theme(legend.title=element_blank(), legend.justification=c(0,1), legend.position=c(0,1), legend.text = element_text(size = 12)) +
  theme(axis.text=element_text(size=12))+
  theme(axis.text.x=element_text(hjust=1, angle=45)) 

do13cFig

# Print DO13C
do13cFig_print <- ggsave(plot = do13cFig, width = 9, height = 12, dpi = 600, filename = "DO13C_ROCKY_QCS01_PRUTH.png")


# by year
# trends between years accross all sites (year plotted separately for each site)
do13cFigYear <- ggplot(data=do13cSum2019, aes(x=month, 
                                              y=ppm_c_doc, 
                                              col= Site,
                                              fill=Site,
                                              ymin=ppm_c_doc-sd,
                                              ymax=ppm_c_doc+sd)) +
  geom_errorbar( position=position_dodge(pd), width=0) +
  ylab("C13 (ppm)") +
  xlab("Month")+ 
  geom_point(position=position_dodge(pd),size=3, pch=21,colour="black") +
  theme(panel.grid.major = element_line(colour = "grey90", linetype = 3)) +
  facet_wrap(~year, ncol = 1)+
  theme(axis.text=element_text(size=12))+
  scale_color_manual(values = cbPalette) +
  scale_fill_manual(values = cbPalette) +
  theme(axis.text.x=element_text(hjust=1)) +
  theme(legend.title=element_blank(), legend.justification=c(0,1), legend.position=c(0,1), legend.text = element_text(size = 12)) +
  ggtitle(~italic("Nearshore Surface Water"), "DO13C") +
  theme(axis.text.x=element_text(hjust=1, angle=45))  


do13cFigYear

# Print DO13C
do13cFig_print <- ggsave(plot = do13cFigYear, width = 8.5, height = 11, dpi = 600, filename = "DO13C_by year.png")



# --- Summary figures end ---

save.image(file= "/Users/lauranliggan/Dropbox/Projects/Hakai Projects/Surface Water Grabs/DO13C/2018/Scripts/DO13C_2018.R")

load(file= "/Users/lauranliggan/Dropbox/Projects/Hakai Projects/Surface Water Grabs/DO13C/2018/Scripts/DO13C_2018.R")


rmarkdown::render('/Users/lauranliggan/Dropbox/Projects/Hakai Projects/Surface Water Grabs/DO13C/2018/Scripts/DO13C_2018.Rmd')

