##################################
# Setup preparation for our codes
# Bruno Paese
# Last updated: June 25, 2025
##################################

## Load packages -------------------------------------------------------------
library(here)

## Define global variables ---------------------------------------------------
# Set global variables
# We decided to use the package viridis to provide the color palettes. To use the colors defined here, uncomment lines 13 and 14, and don't forget to change scale_color_viridis() to scale_color_manual() on the graphs
#colorpalette <- c("#404040", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#332288", "#D55E00", "#CC79A7", "#999933",
#                  "#770000", "#FF3300", "#AAAAAA", "#000000")

linetypes <- c("solid", "22", "42", "44", "13", "1343", "73", "2262", "12223242","F282", "F4448444", "224282F2", "F1", "aa")

# Save the intermediate data set 
save(colorpalette, linetypes, file = here("output","globalvariables.Rdata"))