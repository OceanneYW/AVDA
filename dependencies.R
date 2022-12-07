if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
}

# Bioc packages used
bioc_packages <- c(
    "Biostrings"
)

# Install bioc packages if not installed
new_packages <- bioc_packages[!(bioc_packages %in%
    installed.packages()[, "Package"])]
if (length(new_packages)) BiocManager::install(new_packages)

lapply(bioc_packages, library, character.only = TRUE)

# Standard libraries used
list_of_packages <- c(
    "ape",
    "cluster",
    "ClusterR",
    "dendextend",
    "dplyr",
    "factoextra",
    "ggdendro",
    "ggplot2",
    "ggrepel",
    "grid",
    "RColorBrewer",
    "tidyverse",
    "vegan",
    "shiny",
    "plotly",
    "shinyWidgets"
)

# Install if not installed
new_packages <- list_of_packages[!(list_of_packages %in%
    installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages)

# Load libraries
lapply(list_of_packages, library, character.only = TRUE)
