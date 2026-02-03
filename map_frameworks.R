library(tidyverse)
library(readxl)
library(magrittr)
library(here)

## Read in framework implementation files:
# ESSAT > Framework for the Development of Environment Statistics (FDES) > found at https://unstats.un.org/unsd/envstats/fdes/essat.cshtml
# CISAT > Global Set of Climate Change Statistics and Indicators (GS) > found at https://unstats.un.org/unsd/envstats/Climate%20Change/cisat.cshtml

fdes <- read_excel(here("Frameworks/PartII_ESSAT.xlsm"), sheet = "Component 1")
gs <- read_excel(here("Frameworks/CISAT_Part_2.xlsx"), sheet = "Self assessment tool")
