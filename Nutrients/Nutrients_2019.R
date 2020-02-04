# --- Hakai Surface Water Grabs (Nutrients) data loading and formatting ---
## Author:       Lauran Liggan ##
## Date Created: 2020-02-04    ##
## Notes:      : Part of main summary script and
##               separated to make code more manageable and modular.

# This script contains code for loading and [some simple] tidying of the
#   Hakai Surface Water Grabs data subsets, specificallty water nutrients, as well as any necessary packages. 

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
library(devtools)
library(googlesheets)
library(ggthemes)
library(ggplot2)
library (Rmisc)
library(cowplot)
library(gridExtra)

# --- Packages end ----

# --- Load data ---
#   The following code loads the Nutrients datasets, rename some variables,
#   and create some new variables.

## Set a loading directory ----
# Save all csv files in Nutrients -> GitHub projects 

setwd("~/Desktop/GitHub projects/nearshore_water_grabs/Nutrients") 

getwd()

## Load Nutrient data ----

LL2019_nutrientsData <- read.csv(file="nutrients_2017_2018_2019.csv",
                                 header           = TRUE,
                                 stringsAsFactors = FALSE) %>%
  
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
                                      ifelse(site == "ROCKY03", "5th Beach",
                                      ifelse(site == "ROCKY07", "North Beach",
                                      ifelse(site == "QCS01", "QCS01",
                                      ifelse(site == "WBCH01", "WBCH01", 
                                      ifelse(site == "NBCH01", "NBCH01",
                                      ifelse(site == "PRUTH", "PRUTH",NA))))))))))))

glimpse(LL2019_nutrientsData)

# --- Load data end ---

# --- Clean data ---

## Clean Nutrient dataset ----

#Nutrients2019 <- na.omit(Nutrients2019)
#NROW(Nutrients2019)

LL2019_nutrientsData$month <- factor(LL2019_nutrientsData$month)

LL2019_nutrientsData$month <- factor(LL2019_nutrientsData$month, levels = c("January", "February", "March", "April","May", "June", "July", "August", "September", "October", "November", "December"))

# Create a year column
LL2019_nutrientsData$year<- year(LL2019_nutrientsData$date)

# filter out data above 1m
LL2019_nutrientsData1m<- filter(LL2019_nutrientsData, line_out_depth < 1)
# --- Clean data end ---


# --- Summarize dataset ---
## Summarize DO13C dataset ----

PO4Sum2019<- summarySE(LL2019_nutrientsData1m,
                       measurevar="po4", 
                       groupvars=c("month", "year", "Site",
                                   na.rm=FALSE))
PO4Sum2019

sio2Sum2019<- summarySE(LL2019_nutrientsData1m,
                        measurevar="sio2", 
                        groupvars=c("month", "year", "Site",
                                    na.rm=FALSE))
sio2Sum2019

NitrateSum2019<- summarySE(LL2019_nutrientsData1m,
                           measurevar="no2_no3_um", 
                           groupvars=c("month", "year", "Site",
                                       na.rm=FALSE))
NitrateSum2019



# --- Summary figures ---
## Nutrient summary figures ---- 

# format aesthetics

theme_set(theme_classic(base_size = 16))

pd <- 0.5

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#660066", "#666600")

#Between Sites
# PO4

nutsPO4Fig <- ggplot(PO4Sum2019, aes(x= month, y= po4, 
                                     col=Site, 
                                     fill=Site,
                                     ymin=po4-sd,
                                     ymax=po4+sd)) +
  geom_point(position=position_dodge(pd),size=3, pch=21,colour="black") +
  geom_errorbar( position=position_dodge(pd), width= 0) +
  geom_line( position=position_dodge(pd), aes(group=Site), linetype= 2) +
  theme(panel.grid.major = element_line(colour = "grey90", linetype = 3)) +
  scale_color_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) +
  facet_grid(Site~year) +
  ylab("PO4") +
  xlab("Month") +
  theme(axis.text.x=element_text(hjust=1)) +
  ggtitle(~italic("Nearshore Surface Water"), "Nutrients") + 
  theme(plot.title = element_text(hjust = -0.01, vjust=1.12, size =20)) +
  theme(legend.position="none") +
  theme(axis.text.x=element_text(hjust=1, angle=45))  

nutsPO4Fig

Nuts_print <- ggsave(plot = nutsPO4Fig, width = 10, height = 17, dpi = 600, filename = "PO4_by site.png")



# Si

nutsSiFig <- ggplot(sio2Sum2019, aes(x= month, y= sio2, 
                                     col=Site, 
                                     fill=Site,
                                     ymin=sio2-sd,
                                     ymax=sio2+sd)) +
  geom_point(position=position_dodge(pd),size=3, pch=21,colour="black") +
  geom_errorbar( position=position_dodge(pd), width= 0) +
  geom_line( position=position_dodge(pd), aes(group=Site), linetype= 2) +
  theme(panel.grid.major = element_line(colour = "grey90", linetype = 3)) +
  scale_color_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) +
  facet_grid(Site~year) +
  ylab("SiO2") +
  xlab("Month") +
  theme(axis.text.x=element_text(hjust=1)) +
  ggtitle(~italic("Nearshore Surface Water"), "Nutrients") + 
  theme(plot.title = element_text(hjust = -0.01, vjust=1.12, size =20)) +
  theme(legend.position="none") +
  theme(axis.text.x=element_text(hjust=1, angle=45))  

nutsSiFig

Nuts_print <- ggsave(plot = nutsSiFig, width = 10, height = 17, dpi = 600, filename = "SiO2_by site.png")


# Nitrates

nutsNitFig <- ggplot(NitrateSum2019, aes(x= month, y= no2_no3_um, 
                                         col=Site, 
                                         fill=Site,
                                         ymin=no2_no3_um-sd,
                                         ymax=no2_no3_um+sd)) +
  geom_point(position=position_dodge(pd),size=3, pch=21,colour="black") +
  geom_errorbar( position=position_dodge(pd), width= 0) +
  geom_line( position=position_dodge(pd), aes(group=Site), linetype= 2) +
  theme(panel.grid.major = element_line(colour = "grey90", linetype = 3)) +
  scale_color_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) +
  facet_grid(Site~year) +
  ylab("NO3 + NO2") +
  xlab("Month") +
  theme(axis.text.x=element_text(hjust=1)) +
  ggtitle(~italic("Nearshore Surface Water"), "Nutrients") + 
  theme(plot.title = element_text(hjust = -0.01, vjust=1.12, size =20)) +
  theme(legend.position="none") +
  theme(axis.text.x=element_text(hjust=1, angle=45))  

nutsNitFig

Nuts_print <- ggsave(plot = nutsNitFig, width = 10, height = 17, dpi = 600, filename = "nitrates_by site.png")


# stack all graphs into one figure

library(grid)
gl = lapply(list(nutsPO4Fig, nutsSiFig,nutsNitFig), ggplotGrob) 
library(gtable)
g = do.call(rbind, c(gl, size="first"))
g$widths = do.call(unit.pmax, lapply(gl, "[[", "widths"))
grid.newpage()
grid.draw(g) 


# Print 
Nuts_print <- ggsave(plot = g, width = 8, height = 8, dpi = 600, filename = "Nutrients_by site.png")


#Accross all Sites
# PO4

nutsPO4Fig <- ggplot(LL2019_nutrientsData1m, aes(x= month, y= po4)) +
  geom_boxplot(aes(), size=0.8) +
  geom_jitter(aes(color = Site), size=2.5, alpha = 0.8, position=position_dodge(pd)) +
  scale_color_manual(values = cbPalette) + 
  theme(panel.grid.major = element_line(colour = "grey90", linetype = 3)) +
  ylab("PO4") +
  xlab("Month") +
  facet_wrap(~year) +
  theme(axis.text.x=element_text(hjust=1)) +
  ggtitle(~italic("Nearshore Surface Water"), "Nutrients (<1m depth)") + 
  theme(plot.title = element_text(hjust = -0.01, vjust=1.12, size =20)) +
  theme(legend.title=element_blank(), legend.justification=c(0,1), legend.position=c(0,1), legend.text = element_text(size = 12)) +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank())  

nutsPO4Fig

# Si


nutsSiFig <- ggplot(LL2019_nutrientsData1m, aes(x= month, y= sio2)) +
  geom_boxplot(aes(), size=0.8) +
  geom_jitter(aes(color = Site), size=2.5, alpha = 0.8, position=position_dodge(pd)) +
  scale_color_manual(values = cbPalette) + 
  theme(panel.grid.major = element_line(colour = "grey90", linetype = 3)) +
  facet_wrap(~year) +
  ylab("SiO2") +
  xlab("Month") +
  theme(axis.text.x=element_text(hjust=1)) +
  theme(plot.title = element_text(hjust = -0.01, vjust=1.12, size =20)) +
  theme(legend.position="none")+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank())  

nutsSiFig

# Nitrates

nutsNitFig <- ggplot(LL2019_nutrientsData1m, aes(x= month, y= no2_no3_um)) +
  geom_boxplot(aes(), size=0.8) +
  theme(panel.grid.major = element_line(colour = "grey90", linetype = 3)) +
  geom_jitter(aes(color = Site), size=2.5, alpha = 0.8, position=position_dodge(pd)) +
  scale_color_manual(values = cbPalette) + 
  facet_wrap(~year) +
  ylab("NO3 + NO2") +
  xlab("Month") +
  theme(axis.text.x=element_text(hjust=1)) +
  theme(plot.title = element_text(hjust = -0.01, vjust=1.12, size =20)) +
  theme(legend.position="none")+
  theme(axis.text.x=element_text(hjust=1, angle=45))  

nutsNitFig


# stack all graphs into one figure

library(grid)
gl = lapply(list(nutsPO4Fig, nutsSiFig,nutsNitFig), ggplotGrob) 
library(gtable)
g = do.call(rbind, c(gl, size="first"))
g$widths = do.call(unit.pmax, lapply(gl, "[[", "widths"))
grid.newpage()
grid.draw(g) 

# Print 
Nuts_print <- ggsave(plot = g, width = 12, height = 14, dpi = 600, filename = "Nutrients_by year.png")


# Across All Sites
nutsPO4Fig <- ggplot(PO4Sum2019, aes(x= month, y= po4, 
                                     col=Site, 
                                     fill=Site,
                                     ymin=po4-sd,
                                     ymax=po4+sd)) +
  geom_point(position=position_dodge(pd),size=3, pch=21,colour="black") +
  geom_errorbar( position=position_dodge(pd), width= 0) +
  theme(panel.grid.major = element_line(colour = "grey90", linetype = 3)) +
  scale_color_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) +
  facet_wrap(~year) +
  ylab("PO4") +
  xlab("") +
  theme(axis.text.x=element_text(hjust=1)) +
  ggtitle(~italic("Nearshore Surface Water"), "Nutrients") + 
  theme(plot.title = element_text(hjust = -0.01, vjust=1.12, size =20)) +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank())  +
  theme(legend.title=element_blank(), legend.justification=c(0,1), legend.position=c(0,1), legend.text = element_text(size = 12)) 


nutsPO4Fig


Nuts_print <- ggsave(plot = nutsPO4Fig, width = 10, height = 17, dpi = 600, filename = "PO4_by site.png")



# Si

nutsSiFig <- ggplot(sio2Sum2019, aes(x= month, y= sio2, 
                                     col=Site, 
                                     fill=Site,
                                     ymin=sio2-sd,
                                     ymax=sio2+sd)) +
  geom_point(position=position_dodge(pd),size=3, pch=21,colour="black") +
  geom_errorbar( position=position_dodge(pd), width= 0) +
  theme(panel.grid.major = element_line(colour = "grey90", linetype = 3)) +
  scale_color_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) +
  facet_wrap(~year) +
  ylab("SiO2") +
  xlab("") +
  theme(axis.text.x=element_text(hjust=1)) +
  theme(plot.title = element_text(hjust = -0.01, vjust=1.12, size =20)) +
  theme(legend.position="none") +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank())  

nutsSiFig

Nuts_print <- ggsave(plot = nutsSiFig, width = 10, height = 17, dpi = 600, filename = "SiO2_by site.png")


# Nitrates

nutsNitFig <- ggplot(NitrateSum2019, aes(x= month, y= no2_no3_um, 
                                         col=Site, 
                                         fill=Site,
                                         ymin=no2_no3_um-sd,
                                         ymax=no2_no3_um+sd)) +
  geom_point(position=position_dodge(pd),size=3, pch=21,colour="black") +
  geom_errorbar( position=position_dodge(pd), width= 0) +
  theme(panel.grid.major = element_line(colour = "grey90", linetype = 3)) +
  scale_color_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) +
  facet_wrap(~year) +
  ylab("NO3 + NO2") +
  xlab("Month") +
  theme(axis.text.x=element_text(hjust=1)) +
  theme(plot.title = element_text(hjust = -0.01, vjust=1.12, size =20)) +
  theme(legend.position="none") +
  theme(axis.text.x=element_text(hjust=1, angle=45))  

nutsNitFig

Nuts_print <- ggsave(plot = nutsNitFig, width = 10, height = 17, dpi = 600, filename = "nitrates_by site.png")


# stack all graphs into one figure

library(grid)
gl = lapply(list(nutsPO4Fig, nutsSiFig,nutsNitFig), ggplotGrob) 
library(gtable)
g = do.call(rbind, c(gl, size="first"))
g$widths = do.call(unit.pmax, lapply(gl, "[[", "widths"))
grid.newpage()
grid.draw(g) 

Nuts_print <- ggsave(plot = g, width = 14, height = 18, dpi = 600, filename = "nutrients_all sites.png")


# --- Summary figures end ---


