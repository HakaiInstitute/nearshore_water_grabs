# --- Hakai Surface Water Grabs (Nutrients) data loading and formatting ---
## Author:       Lauran Liggan ##
## Date Created: 2018-10-23    ##
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
# Save all csv files in Nutrients -> 2017 -> Data folder on Dropbox

setwd("~/Desktop/GitHub projects/nearshore_water_grabs/Nutrients") 

getwd()

## Load Nutrient data ----

LL2017_nutrientsData <- read.csv(file="Nutrients_ROCKY_cleaned_LL.csv",
                                 header           = TRUE,
                                 stringsAsFactors = FALSE) %>%
  
  mutate(Site             = as.factor(ifelse(Site == "ROCKY02", "Foggy Cove",
                                      ifelse(Site == "ROCKY04", "Little Wolf", 
                                      ifelse(Site == "ROCKY05", "Nalau",
                                      ifelse(Site == "ROCKY03", "5th Beach",
                                      ifelse(Site == "ROCKY06", "West Beach",
                                      ifelse(Site == "ROCKY07", "North Beach",NA))))))))

glimpse(LL2017_nutrientsData)

# --- Load data end ---

# --- Clean data ---

## Clean Nutrient dataset ----

Nutrients2017 <- na.omit(LL2017_nutrientsData)
NROW(Nutrients2017)

Nutrients2017$month <- factor(Nutrients2017$month)

Nutrients2017$month <- factor(Nutrients2017$month, levels = c("April","May", "June", "July", "August"))

# --- Clean data end ---

# --- Summary figures ---
## Nutrient summary figures ---- 

# format aesthetics

theme_set(theme_classic(base_size = 16))

pd <- 0.5

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#Between Sites
# PO4

nutsPO4Fig <- ggplot(Nutrients2017, aes(x= month, y= PO4, col=Site)) +
  geom_point( position=position_dodge(pd), size =3) +
  geom_line( position=position_dodge(pd), aes(group=Site), linetype= 2) +
  theme(panel.grid.major = element_line(colour = "grey90", linetype = 3)) +
  scale_color_manual(values = cbPalette) + 
  ylab("PO4") +
  xlab("Month") +
  facet_wrap(~Site, ncol = 6) +
  theme(axis.text.x=element_text(hjust=1)) +
  ggtitle(~italic("Nearshore Surface Water (2017)"), "Nutrients") + 
  theme(plot.title = element_text(hjust = -0.01, vjust=1.12, size =20)) +
  theme(legend.position="none")+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank())  

nutsPO4Fig


# Si

nutsSiFig <- ggplot(Nutrients2017, aes(x= month, y= SiO2, col=Site)) +
  geom_point( position=position_dodge(pd), size=3) +
  geom_line( position=position_dodge(pd), aes(group=Site), linetype= 2) +
  theme(panel.grid.major = element_line(colour = "grey90", linetype = 3)) +
  scale_color_manual(values = cbPalette) + 
  ylab("SiO2") +
  xlab("Month") +
  scale_y_continuous(breaks = seq(0, 15, len = 6)) +
  facet_wrap(~Site, ncol = 6) +
  theme(strip.background = element_blank(), strip.text.x = element_text(size=0)) +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank()) +
  theme(legend.position="none")

nutsSiFig

# Nitrates

nutsNitFig <- ggplot(Nutrients2017, aes(x= month, y= NO3.NO2, col=Site)) +
  geom_point( position=position_dodge(pd), size=3) +
  geom_line( position=position_dodge(pd), aes(group=Site), linetype= 2) +
  theme(panel.grid.major = element_line(colour = "grey90", linetype = 3)) +
  scale_color_manual(values = cbPalette) + 
  ylab("NO3 + NO2") +
  facet_wrap(~Site, ncol = 6) +
  
  xlab("Month") +
  scale_y_continuous(breaks = seq(0, 8, len = 5)) +
  theme(strip.background = element_blank(), strip.text.x = element_text(size=0)) +
  theme(axis.text.x=element_text(hjust=1)) +
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
Nuts_print <- ggsave(plot = g, width = 8, height = 8, dpi = 600, filename = "Nutrients_by site.png")


#Accross all Sites
# PO4

nutsPO4Fig <- ggplot(Nutrients2017, aes(x= month, y= PO4)) +
  geom_boxplot(col= "#009E73") +
  theme(panel.grid.major = element_line(colour = "grey90", linetype = 3)) +
  scale_color_manual(values = cbPalette) + 
  ylab("PO4") +
  xlab("Month") +
  theme(axis.text.x=element_text(hjust=1)) +
  ggtitle(~italic("Nearshore Surface Water (2017)"), "Nutrients") + 
  theme(plot.title = element_text(hjust = -0.01, vjust=1.12, size =20)) +
  theme(legend.position="none")+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank())  

nutsPO4Fig

# Si

nutsSiFig <- ggplot(Nutrients2017, aes(x= month, y= SiO2)) +
  geom_boxplot(col= "#999999") +
  theme(panel.grid.major = element_line(colour = "grey90", linetype = 3)) +
  scale_color_manual(values = cbPalette) + 
  ylab("SiO2") +
  xlab("Month") +
  scale_y_continuous(breaks = seq(0, 15, len = 6)) +
  theme(strip.background = element_blank(), strip.text.x = element_text(size=0)) +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank()) +
  theme(legend.position="none")

nutsSiFig

# Nitrates

nutsNitFig <- ggplot(Nutrients2017, aes(x= month, y= NO3.NO2)) +
                       geom_boxplot(col="#D55E00") +
  theme(panel.grid.major = element_line(colour = "grey90", linetype = 3)) +
  scale_color_manual(values = cbPalette) + 
  ylab("NO3 + NO2") +
  xlab("Month") +
  scale_y_continuous(breaks = seq(0, 8, len = 5)) +
  theme(strip.background = element_blank(), strip.text.x = element_text(size=0)) +
  theme(axis.text.x=element_text(hjust=1)) +
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
Nuts_print <- ggsave(plot = g, width = 6, height = 6, dpi = 600, filename = "Nutrients_all sites.png")

# --- Summary figures end ---


