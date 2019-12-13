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
# Save all csv files in CHLa -> GitHub projects

setwd("~/Desktop/GitHub projects/nearshore_water_grabs/CHLa") 

getwd()

## Load DO13C data ----

LL2019_CHLaData <- read.csv(file="CHLa data_2019.csv",
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
                                             
   mutate(plankton        = as.factor(ifelse(Plankton.Size  == "20um", "Nano",
                                       ifelse(Plankton.Size == "3um", "Pico", 
                                       ifelse(Plankton.Size == "Bulk GF/F", "Micro Bulk",
                                       ifelse(Plankton.Size == "GF/F", "Micro", NA )))))) %>%                             
  
  mutate(Site             = as.factor(ifelse(site == "ROCKY02", "Foggy Cove",
                                      ifelse(site == "ROCKY04", "Little Wolf", 
                                      ifelse(site == "ROCKY05", "Nalau",
                                      ifelse(site == "ROCKY06", "West Beach",
                                      ifelse(site == "ROCKY07", "North Beach",
                                      ifelse(site == "ROCKY03", "5th Beach",
                                      ifelse(site == "PRUTH", "Pruth Bay",
                                      ifelse(site == "QCS01", "QCS01",
                                      ifelse(site == "WBCH01", "WBCH01", 
                                      ifelse(site == "NBCH01", "NBCH01",
                                      ifelse(site == "PRUTH", "PRUTH",NA)))))))))))))

glimpse(LL2019_CHLaData)

# --- Load data end ---

# --- Clean data ---

## Clean DO13C dataset ----

CHla2019 <- na.omit(LL2019_CHLaData)
NROW(CHla2019)

# recode late 2017 June months to "07"

LL2019_CHLaData$monthNum <- as.character(LL2019_CHLaData$monthNum )

LL2019_CHLaData$monthNum[ LL2019_CHLaData$date > ymd( "2017-06-15" ) & 
                     LL2019_CHLaData$date < ymd( "2017-07-01" ) ] <- "07"

LL2019_CHLaData$monthNum <- factor(LL2019_CHLaData$monthNum )

LL2019_CHLaData$month <- as.character( LL2019_CHLaData$month)
LL2019_CHLaData$month[ LL2019_CHLaData$date > ymd( "2017-06-15" ) & 
                  LL2019_CHLaData$date < ymd( "2017-07-01" ) ] <- "July"

# Factor month in proper order
LL2019_CHLaData$month <- factor(LL2019_CHLaData$month)

LL2019_CHLaData$month <- factor(LL2019_CHLaData$month, levels = c("January", "February", "March", "April","May", "June", "July", "August", "September", "October", "November", "December"))

# Create a year column
LL2019_CHLaData$year<- year(LL2019_CHLaData$date)

# View data
glimpse(LL2019_CHLaData)

# filter out data above 1m
LL2019_CHLaData1m<- filter(LL2019_CHLaData, line_out_depth < 1, Phaeo < 10)

# --- Clean data end ---

# --- Summarize dataset ---
## Summarize DO13C dataset ----

CHlaSum2019<- summarySE(LL2019_CHLaData1m,
                         measurevar="Chla", 
                         groupvars=c("month", "year", "Site", "plankton", 
                                     na.rm=FALSE))
CHlaSum2019

PhaeoSum2019<- summarySE(LL2019_CHLaData1m,
                        measurevar="Phaeo", 
                        groupvars=c("month", "year", "Site", "plankton", 
                                    na.rm=FALSE))
PhaeoSum2019



# --- Summarize dataset end ---

# --- Summary figures ---
## DO13C summary figures ---- 

# format aesthetics

theme_set(theme_classic(base_size = 16))

pd <- 0.5

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#660066", "#666600")


#CHLa
# by site
# trends between sites

CHLaFigSites <- ggplot(data=CHlaSum2019, aes(x=month, 
                                              y=Chla, 
                                              col= plankton,
                                              ymin=Chla-se,
                                              ymax=Chla+se)) +
  geom_errorbar( position=position_dodge(pd), width=.5) +
  ylab("Chlorophyll (microg/L)") +
  xlab("Month")+ 
  geom_point( position=position_dodge(pd), size=3) +
  theme(panel.grid.major = element_line(colour = "grey90", linetype = 3)) +
  geom_line( position=position_dodge(pd), aes(group=plankton, linetype= plankton)) +
  facet_grid(Site~year)+
  theme(axis.text=element_text(size=12))+
  scale_color_manual(values = cbPalette) +
  theme(axis.text.x=element_text(hjust=1)) +
  theme(legend.justification=c(0,1), legend.position=c(0.01,0.99), legend.text = element_text(size = 12)) +
  ggtitle(~italic("Nearshore Surface Water"), "CHLa") +
  theme(axis.text.x=element_text(hjust=1, angle=45))  

CHLaFigSites

# Print DO13C
CHLaFig_print <- ggsave(plot = CHLaFigSites, width = 15, height = 17, dpi = 600, filename = "CHLa_2019_by site.png")



#dot plot of all sites
CHLaPlot <- ggplot(data=LL2019_CHLaData1m, aes(x=month, 
                                     y=Chla, col=Site)) +
  geom_point(position=position_dodge(pd),size=3) +

  scale_fill_manual(values = cbPalette) +
  scale_color_manual(values = cbPalette) +
  facet_grid(year~plankton) +
  ggtitle(~italic("Nearshore Surface Water"), "CHLa") +
  ylab("Chlorophyll (microg/L)") +
  xlab("Month") +
  theme(panel.grid.major = element_line(colour = "grey90", linetype = 3)) +
  theme(axis.text.x=element_text(hjust=1)) +
  theme(legend.justification=c(0,1), legend.position=c(0.01,0.97), legend.text = element_text(size = 12), legend.background = element_rect(linetype = 2, size = 0.5, colour = 1)) +
  theme(plot.title = element_text(hjust = -0.01, vjust=1.12, size =20)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

CHLaPlot

# Print 
CHla2018Fig_print <- ggsave(plot =  CHLaPlot, width = 12, height = 15, dpi = 600, filename = "CHLaPlot_2019_all sites.png")

# trends accross all sites


CHla2017Fig <- ggplot(data=subset(CHlaSum2019, year == "2017"), aes(month, Chla, col=plankton)) +
  geom_boxplot() + 
  theme(panel.grid.major = element_line(colour = "grey90", linetype = 3)) +
  scale_color_manual(values = cbPalette) + 
  ylab("Chlorophyll (microg/L)") +
  xlab("Month")+ 
  theme(axis.text.x=element_text(hjust=1)) +
  theme(axis.text.y=element_text(hjust=1)) +
  ggtitle("CHLa 2017 (<1m depth)") + 
  theme(plot.title = element_text(hjust = -0.01, vjust=1.12, size =20)) +
  theme(legend.justification=c(0,1), legend.position=c(0.8,0.97),legend.text = element_text(size = 12)) +
  theme(axis.text=element_text(size=12))+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank())  


CHla2017Fig


CHla2018Fig <- ggplot(data=subset(CHlaSum2019, year == "2018"), aes(month, Chla, col=plankton)) +
  geom_boxplot() + 
  theme(panel.grid.major = element_line(colour = "grey90", linetype = 3)) +
  scale_color_manual(values = cbPalette) + 
  ylab("Chlorophyll (microg/L)") +
  xlab("Month")+ 
  theme(axis.text.x=element_text(hjust=1)) +
  theme(axis.text.y=element_text(hjust=1)) +
  ggtitle("CHLa 2018") + 
  theme(plot.title = element_text(hjust = -0.01, vjust=1.12, size =20)) +
  theme(legend.position="none") +
  theme(axis.text=element_text(size=12))+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank())  


CHla2018Fig


CHla2019Fig <- ggplot(data=subset(CHlaSum2019, year == "2019"), aes(month, Chla, col=plankton)) +
  geom_boxplot() + 
  theme(panel.grid.major = element_line(colour = "grey90", linetype = 3)) +
  scale_color_manual(values = cbPalette) + 
  ylab("Chlorophyll (microg/L)") +
  xlab("Month")+ 
  theme(axis.text.x=element_text(hjust=1)) +
  theme(axis.text.y=element_text(hjust=1)) +
  ggtitle("CHLa 2019") + 
  theme(plot.title = element_text(hjust = -0.01, vjust=1.12, size =20)) +
  theme(legend.position="none") +
  theme(axis.text=element_text(size=12))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


CHla2019Fig

# stack all graphs into one figure

library(grid)
gl = lapply(list(CHla2017Fig, CHla2018Fig,CHla2019Fig), ggplotGrob) 
library(gtable)
g = do.call(rbind, c(gl, size="first"))
g$widths = do.call(unit.pmax, lapply(gl, "[[", "widths"))
grid.newpage()
grid.draw(g) 

# Print 
CHla2019Fig_print <- ggsave(plot = g, width = 7, height = 10, dpi = 600, filename = "CHLa_2019_all sites.png")


CHLa2019Fig <- ggplot(LL2019_CHLaData1m, aes(x= month, y= Chla)) +
  geom_boxplot(aes(), size=0.8) +
  geom_jitter(aes(color = Site), size=2, alpha = 0.5, position=position_dodge(pd)) +
  scale_color_manual(values = cbPalette) + 
  theme(panel.grid.major = element_line(colour = "grey90", linetype = 3)) +
  ylab("Chlorophyll (microg/L)") +
  xlab("Month")+ 
  facet_grid(year~plankton) +
  theme(axis.text.x=element_text(hjust=1)) +
  ggtitle(~italic("Nearshore Surface Water"), "CHLa (<1m depth)") + 
  theme(plot.title = element_text(hjust = -0.01, vjust=1.12, size =20)) +
  theme(legend.title=element_blank(), legend.justification=c(0,1), legend.position=c(0.01,1), legend.text = element_text(size = 12)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


CHLa2019Fig

# Print 
CHla2018Fig_print <- ggsave(plot =  CHLa2019Fig, width = 12, height = 13, dpi = 600, filename = "CHLaPlot_2019.png")


#PHAEO
# by site
# trends between sites

PhaeoFigSites <- ggplot(data=PhaeoSum2019, aes(x=month, 
                                             y=Phaeo, 
                                             col= plankton,
                                             ymin=Phaeo-se,
                                             ymax=Phaeo+se)) +
  geom_errorbar( position=position_dodge(pd), width=.5) +
  ylab("Phaeo (microg/L)") +
  xlab("Month")+ 
  geom_point( position=position_dodge(pd), size=3) +
  theme(panel.grid.major = element_line(colour = "grey90", linetype = 3)) +
  geom_line( position=position_dodge(pd), aes(group=plankton, linetype= plankton)) +
  facet_grid(Site~year)+
  theme(axis.text=element_text(size=12))+
  scale_color_manual(values = cbPalette) +
  theme(axis.text.x=element_text(hjust=1)) +
  theme(legend.justification=c(0,1), legend.position=c(0.01,0.99), legend.text = element_text(size = 12)) +
  ggtitle(~italic("Nearshore Surface Water"), "Phaeopigment") +
  theme(axis.text.x=element_text(hjust=1, angle=45))  

PhaeoFigSites

# Print DO13C
PhaeoFig_print <- ggsave(plot = PhaeoFigSites, width = 15, height = 17, dpi = 600, filename = "Phaeo_2019_by site.png")



#dot plot of all sites
PhaeoPlot <- ggplot(data=LL2019_CHLaData1m, aes(x=month, 
                                               y=Phaeo, col=Site)) +
  geom_point(position=position_dodge(pd),size=3) +
  
  scale_fill_manual(values = cbPalette) +
  scale_color_manual(values = cbPalette) +
  facet_grid(year~plankton) +
  ggtitle(~italic("Nearshore Surface Water"), "Phaeopigment") +
  ylab("Phaeo (microg/L)") +
  xlab("Month")+ 
  theme(panel.grid.major = element_line(colour = "grey90", linetype = 3)) +
  theme(axis.text.x=element_text(hjust=1)) +
  theme(legend.justification=c(0,1), legend.position=c(0.01,0.97), legend.text = element_text(size = 12), legend.background = element_rect(linetype = 2, size = 0.5, colour = 1)) +
  theme(plot.title = element_text(hjust = -0.01, vjust=1.12, size =20)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

PhaeoPlot

# Print 
PhaeoPlotFig_print <- ggsave(plot =  PhaeoPlot, width = 12, height = 15, dpi = 600, filename = "PhaeoPlot_2019_all sites.png")

# trends accross all sites


Phaeo2017Fig <- ggplot(data=subset(PhaeoSum2019, year == "2017"), aes(month, Phaeo, col=plankton)) +
  geom_boxplot() + 
  theme(panel.grid.major = element_line(colour = "grey90", linetype = 3)) +
  scale_color_manual(values = cbPalette) + 
  ylab("Phaeo (microg/L)") +
  xlab("Month")+ 
  theme(axis.text.x=element_text(hjust=1)) +
  theme(axis.text.y=element_text(hjust=1)) +
  ggtitle("Phaeopigments 2017 (<1m depth)") + 
  theme(plot.title = element_text(hjust = -0.01, vjust=1.12, size =20)) +
  theme(legend.justification=c(0,1), legend.position=c(0.8,0.97),legend.text = element_text(size = 12)) +
  theme(axis.text=element_text(size=12))+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank())  


Phaeo2017Fig


Phaeo2018Fig <- ggplot(data=subset(PhaeoSum2019, year == "2018"), aes(month, Phaeo, col=plankton)) +
  geom_boxplot() + 
  theme(panel.grid.major = element_line(colour = "grey90", linetype = 3)) +
  scale_color_manual(values = cbPalette) + 
  ylab("Phaeo (microg/L)") +
  xlab("Month")+ 
  theme(axis.text.x=element_text(hjust=1)) +
  theme(axis.text.y=element_text(hjust=1)) +
  ggtitle("Phaeopigments 2018") + 
  theme(plot.title = element_text(hjust = -0.01, vjust=1.12, size =20)) +
  theme(legend.position="none") +
  theme(axis.text=element_text(size=12))+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank())  


Phaeo2018Fig


Phaeo2019Fig <- ggplot(data=subset(PhaeoSum2019, year == "2019"), aes(month, Phaeo, col=plankton)) +
  geom_boxplot() + 
  theme(panel.grid.major = element_line(colour = "grey90", linetype = 3)) +
  scale_color_manual(values = cbPalette) + 
  ylab("Phaeo (microg/L)") +
  xlab("Month")+ 
  theme(axis.text.x=element_text(hjust=1)) +
  theme(axis.text.y=element_text(hjust=1)) +
  ggtitle("Phaeopigments 2019") + 
  theme(plot.title = element_text(hjust = -0.01, vjust=1.12, size =20)) +
  theme(legend.position="none") +
  theme(axis.text=element_text(size=12))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


Phaeo2019Fig

# stack all graphs into one figure

library(grid)
gl = lapply(list(Phaeo2017Fig, Phaeo2018Fig,Phaeo2019Fig), ggplotGrob) 
library(gtable)
g = do.call(rbind, c(gl, size="first"))
g$widths = do.call(unit.pmax, lapply(gl, "[[", "widths"))
grid.newpage()
grid.draw(g) 

# Print 
Phaeo2019Fig_print <- ggsave(plot = g, width = 7, height = 10, dpi = 600, filename = "Phaeo_2019_all sites.png")


Phaeo2019Fig <- ggplot(LL2019_CHLaData1m, aes(x= month, y= Phaeo)) +
  geom_boxplot(aes(), size=0.8) +
  geom_jitter(aes(color = Site), size=2, alpha = 0.5, position=position_dodge(pd)) +
  scale_color_manual(values = cbPalette) + 
  theme(panel.grid.major = element_line(colour = "grey90", linetype = 3)) +
  ylab("Phaeo (microg/L)") +
  xlab("Month")+ 
  facet_grid(year~plankton) +
  theme(axis.text.x=element_text(hjust=1)) +
  ggtitle(~italic("Nearshore Surface Water"), "Phaeopigments (<1m depth)") + 
  theme(plot.title = element_text(hjust = -0.01, vjust=1.12, size =20)) +
  theme(legend.title=element_blank(), legend.justification=c(0,1), legend.position=c(0.01,1), legend.text = element_text(size = 12)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


Phaeo2019Fig

# Print 
Phaeo2019Fig_print <- ggsave(plot =  Phaeo2019Fig, width = 12, height = 13, dpi = 600, filename = "PhaeoPlot_2019.png")


# --- Summary figures end ---