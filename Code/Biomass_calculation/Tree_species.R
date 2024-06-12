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
library(lattice)
library(corrplot)
library(emmeans)
library(lmerTest)
library(kgc)
library(dplyr)
library(readxl)

#---------------------------------------------------------------------------------------------------------------------------------------
# DATI

# questi dati provengono dalla repository di Github del pacchetto allobd

load("sitespecies.rda")
load("equations.rda")
tab_koppen <- read_excel("tab_koppen.xlsx")
#View(tab_koppen)


# questi sono i miei dati prelevati dalle matrici 

speci <- (equations$equation_taxa)
equazioni <- (equations$equation_allometry)
clima <- (equations$koppen)
unità <-(equations$output_units_original)
dbhunità <- (equations$dbh_units_original)

# li rendo una matrice
dat <- tibble(speci,clima,equazioni,unità,dbhunità)

#---------------------------------------------------------------------------------------------------------------------------------------
# SCELTA DELLA REGIONE

# matrice di koppen -> scelta 

opzioni <- c("Abruzzo","Basilicata","Calabria","Campania","Emilia-Romagna","Friuli-Venezia Giulia","Lazio","Liguria","Lombardia",
             "Marche","Molise","Piemonte","Puglia","Sardegna","Sicilia","Toscana","Trentino-Alto Adige","Umbria","Valle d’Aosta",
             "Veneto")

# faccio scegliere all'utente la regione
regione <- menu(opzioni, title = "Seleziona un'opzione: ")
print(paste("Hai scelto:", opzioni[regione]))

#---------------------------------------------------------------------------------------------------------------------------------------
# SCREMATURA DEL DATASET

# ricerco le posizioni utilizzando le climate zones 

# koppen climate zones 
# Af, Am, Aw/As, BWh, BWk, BSh, BSk, Csa, Csb, Csc, Cwa, Cwb, Cwc, Cfa,
# Cfb, Cfc, Dsa, Dsb, Dsc, Dsd, Dwa, Dwb, Dwc, Dwd, Dfa, Dfb, Dfc, Dfd, ET, EF

scelta_clima = tab_koppen$`climate zone`[regione]
print(scelta_clima)

# divido il vettore
scelta_clima <- unlist(strsplit(scelta_clima, ";"))

# prealloco
posizioni <- c()

# per ogni valore del vettore scelta_clima cerco le posizioni
for(x in scelta_clima){
  pos <- grep(x,dat$clima)
  posizioni <- c(posizioni, pos)
}

# metto a posto il vettore posizioni, in ordine crescente e senza le ripetizioni
posizioni <- sort(posizioni)
posizioni <- unique(posizioni)


# inserisco il vettore char di speci presenti nella foresta

speci_insert <- c("Sapindacee","","");





#---------------------------------------------------------------------------------------------------------------------------------------
# esempio con ippocastano -> aesculus hippocastanus, famiglia Sapindaceae -> ho delle equazioni 

hip_pos <- grep("Sapindaceae",dat$speci)

# prealloco
eq_hip <- c()

for(x in hip_pos) {
  print(x)
  eq <- dat$equazioni[x]
  eq_hip <- c(eq_hip,eq)
}

eq_hip <- sort(eq_hip)
eq_hip <- unique(eq_hip)

n = length(eq_hip)


# diametri presi random ippocastani

dbh <- c(25, 30, 22, 28, 33, 20, 26, 31, 24, 29) #[m]


# lo rendo un vettore numerico

eq_hip_fin <- c() # prealloco

for(x in dbh) {
  eq_hip <- sapply(eq_hip, function(x) eval(parse(text = x)))
  #print(eq_hip)
  eq_hip_fin <- c(eq_hip_fin,eq_hip)
}

eq_hip =eq_hip_fin
eq_hip <- unique(eq_hip)

e = split(eq_hip,rep(1:n,each = length(dbh)))

vec1 = e$`1`
vec2 = e$`2`



#dbh = as.character(dbh)


#---------------------------------------------------------------------------------------------------------------------------------------
# BAR PLOTTING


barplot(vec1, names.arg = dbh, col = "#556B2F",
        main = "Biomass calculation for different hippocastanus trees",
        xlab = "Tree diameter", ylab = "Biomass [Kg]")


barplot(vec2, names.arg = dbh, col = "#9ACD32",
        main = "Biomass calculation for different hippocastanus trees",
        xlab = "Tree diameter [m]", ylab = "Biomass [Kg]")


#---------------------------------------------------------------------------------------------------------------------------------------
# BOX PLOTTING

boxplot(cbind(vec1,vec2), main = "Biomass calculation for different hippocastanus trees",
        xlab = "Tree", ylab = "Biomass [Kg]",
        col = c("#9ACD32", "#556B2F"), border = "black", notch = FALSE, xaxt = "n")


legend("topright", legend = c("eq_1", "eq_2"), fill = c("#9ACD32", "#556B2F"))


#---------------------------------------------------------------------------------------------------------------------------------------
# Larix Cfa -> 52 + Acer saccharum -> 181


lar_pos <- which(dat$speci == "Larix")

lar_pos <- lar_pos[1]
print(lar_pos)


ac_pos <- which(dat$speci == "Pinus palustris")

print(ac_pos)

eq_lar = dat$equazioni[lar_pos]
eq_ac = dat$equazioni[lar_pos]

u_lar = dat$unità[lar_pos]
u_ac = dat$unità[ac_pos]

uu_lar = dat$dbhunità[lar_pos]
uu_ac = dat$dbhunità[ac_pos]

dbh <- c(24, 73, 42, 11, 86, 57, 35, 68, 19, 91) #[cm]

e_lar <- c()

for(x in 1) {
  eq_lar <- sapply(eq_lar, function(x) eval(parse(text = x)))
  print(eq_lar)
  e_lar <- c(eq_lar)
}

barplot(e_lar, names.arg = dbh, col = "#556B2F",
        main = "Biomass calculation for Larix",
        xlab = "Tree diameter [cm]", ylab = "Biomass [kg]")




dbh <- c(24, 50, 42, 11, 45, 19, 35, 30, 23, 49) 


e_ac <- c()

for(x in 1) {
  eq_ac <- sapply(eq_ac, function(x) eval(parse(text = x)))
  print(eq_ac)
  e_ac <- c(eq_ac)
}




barplot(e_ac, names.arg = dbh, col = "#9ACD32",
        main = "Biomass calculation for Pinus palustris",
        xlab = "Tree diameter [cm]", ylab = "Biomass [Kg]")



boxplot(cbind(e_lar,e_ac), main = "Biomass calculation",
        xlab = "Tree", ylab = "Biomass",
        col = c("#556B2F","#9ACD32"), border = "black", notch = FALSE, xaxt = "n")


legend("topright", legend = c("Larix", "Pinus palustris"), fill = c("#556B2F","#9ACD32"))



