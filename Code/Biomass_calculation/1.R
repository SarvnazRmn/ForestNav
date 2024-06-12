# Load required libraries
library(Matrix)
library(lme4)
library(lmerTest)
library(effects)
library(plotrix)
library(gridExtra)
library(piecewiseSEM)
library(nlme)
library(pgirmess)
library(splines)
library(car)
library(ISLR)
library(lattice)
library(latticeExtra)
library(HH)
library(multcomp)
library(openxlsx)
library(multcompView)
library(dplyr)
library(ggplot2)
library(ARTool)
library(sciplot)
library(MASS)
library(nortest)
library(performance)
library(ggpubr)
library(devtools)
library(factoextra)
library(vegan)
library(permute)
library(corrplot)
library(emmeans)
library(kgc)
library(readxl)

#---------------------------------------------------------------------------------------------------------------------------------------
# Load data

load("C:/Users/USER/Downloads/R/sitespecies.rda")
load("C:/Users/USER/Downloads/R/equations.rda")
tab_koppen <- read_excel("C:/Users/USER/Downloads/R/tab_koppen.xlsx")

# Prepare the data
species <- equations$equation_taxa
equations <- equations$equation_allometry
climate <- equations$koppen
units <- equations$output_units_original
dbh_units <- equations$dbh_units_original

# Combine data into a data frame
dat <- data_frame(speci, clima, equazioni, unità, dbhunità)

#---------------------------------------------------------------------------------------------------------------------------------------
# User input for region

opzioni <- c("Abruzzo", "Basilicata", "Calabria", "Campania", "Emilia-Romagna", "Friuli-Venezia Giulia", "Lazio", "Liguria", "Lombardia",
             "Marche", "Molise", "Piemonte", "Puglia", "Sardegna", "Sicilia", "Toscana", "Trentino-Alto Adige", "Umbria", "Valle d’Aosta",
             "Veneto")

regione <- menu(opzioni, title = "Seleziona un'opzione: ")
print(paste("Hai scelto:", opzioni[regione]))

#---------------------------------------------------------------------------------------------------------------------------------------
# Filter dataset by selected region's climate

scelta_clima <- tab_koppen$`climate zone`[regione]
scelta_clima <- unlist(strsplit(scelta_clima, ";"))

posizioni <- c()
for(x in scelta_clima){
  pos <- grep(x, dat$clima)
  posizioni <- c(posizioni, pos)
}
posizioni <- sort(unique(posizioni))

# Filtered dataset based on climate zone
filtered_dat <- dat[posizioni, ]
# Retrieve the climate zone associated with the selected region from the 'tab_koppen' data
selected_climate = tab_koppen$`climate zone`[region]

# Split the climate zone string into a vector of individual climate zones
selected_climate <- unlist(strsplit(selected_climate, ";"))

# Initialize an empty vector to store positions
positions <- c()

# For each climate zone in the selected climate vector, find matching positions in the 'dat' tibble
for (climate in selected_climate) {
  pos <- grep(climate, dat$climate)
  positions <- c(positions, pos)
}

# Sort the positions in ascending order and remove duplicates
positions <- sort(positions)
positions <- unique(positions)
#---------------------------------------------------------------------------------------------------------------------------------------
# User input for tree species and DBH

specie_scelta <- readline(prompt = "Inserisci il nome della specie: ")
dbh_values <- as.numeric(unlist(strsplit(readline(prompt = "Inserisci i valori DBH separati da virgola: "), ",")))

#---------------------------------------------------------------------------------------------------------------------------------------
# Filter equations for selected species

species_pos <- grep(specie_scelta, filtered_dat$speci)

if (length(species_pos) == 0) {
  stop("Specie non trovata nel dataset.")
}

eq_species <- filtered_dat$equazioni[species_pos]

#---------------------------------------------------------------------------------------------------------------------------------------
# Calculate biomass using selected equations

biomass_results <- lapply(eq_species, function(eq) {
  sapply(dbh_values, function(dbh) {
    eval(parse(text = eq))
  })
})

# Convert list to a matrix
biomass_matrix <- do.call(cbind, biomass_results)

#---------------------------------------------------------------------------------------------------------------------------------------
# barplot(vec1, names.arg = dbh, col = "#556B2F",
main = "Biomass calculation for different hippocastanus trees"
xlab = "Tree diameter" 
ylab = "Biomass [Kg]"

barplot(vec2, names.arg = dbh, col = "#9ACD32",
        main = "Biomass calculation for different hippocastanus trees",
        xlab = "Tree diameter [m]", ylab = "Biomass [Kg]")

boxplot(cbind(vec1, vec2), main = "Biomass calculation for different hippocastanus trees",
        xlab = "Tree", ylab = "Biomass [Kg]",
        col = c("#9ACD32", "#556B2F"), border = "black", notch = FALSE, xaxt = "n")

legend("topright", legend = c("eq_1", "eq_2"), fill = c("#9ACD32", "#556B2F"))

