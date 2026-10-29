##Welcome

## All the code used for the paper : "Contrasting life history traits in two genetically distinct 
# Atlantic salmon (Salmo salar) populations from the same watershed" is available here
# Some readme will help you proceed with the analysis
# Have fun



###____________________________________________________________________________________________________________________________________
#Charging package

library(ggplot2)###
library(ggspatial)###
library(sf)###
library(rnaturalearth)###
library(rnaturalearthdata)###
library(ggh4x)###
library(dplyr)###
library(tidyr)###
library(FSA)###
library(nlstools)###
library(cowplot)###
library(ggpubr)###
library(lme4)###
library(performance)###
library(MuMIn)###
library(sjPlot)###
library(corrplot)###
library(adegenet)###
library(hierfstat)###
library(ade4)###
#library(pophelper)###
library(pegas)###
library(klaR)###
library(lubridate)###
library(tibble)###
library(viridis)###
library(MASS)###
library(car)###
library(tidyverse)###
library(lmerTest)###
library(assignPOP)###
library(lubridate)
library(starmie)
######Making the map (Figure 1A)_______________________________________________________________________________________________________________
setwd("~/Sauvegarde/Projet Romaine - Contrat/Chapitres/Chapitre I - Traits d'histoire de vie/Map")
#Map of the world
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
ggplot(data = world) +
  geom_sf()

#Import shapefile
watercourses <- st_read("~/Sauvegarde/Projet Romaine - Contrat/Chapitres/Chapitre I - Traits d'histoire de vie/Map/Shapefile")
bv <- st_read("~/Sauvegarde/Projet Romaine - Contrat/Chapitres/Chapitre I - Traits d'histoire de vie/Map/ShapefileBV")
bv2 <- st_read("~/Sauvegarde/Projet Romaine - Contrat/Chapitres/Chapitre I - Traits d'histoire de vie/Map/ShapefileBV2")
mig <- read.csv("~/Sauvegarde/Projet Romaine - Contrat/Chapitres/Chapitre I - Traits d'histoire de vie/Map/mig.csv",header = T, sep = ";")
mig <- st_as_sf(mig, coords = c("long", "lat"), crs = 4326)
parr <- read.csv("~/Sauvegarde/Projet Romaine - Contrat/Chapitres/Chapitre I - Traits d'histoire de vie/Map/parr.csv",header = T, sep = ";")
parr <- st_as_sf(parr, coords = c("long", "lat"), crs = 4326)
smolt1 <- read.csv("~/Sauvegarde/Projet Romaine - Contrat/Chapitres/Chapitre I - Traits d'histoire de vie/Map/smolt_obj1.csv",header = T, sep = ";")
smolt1 <- st_as_sf(smolt1, coords = c("long", "lat"), crs = 4326)
alevins <- read.csv("~/Sauvegarde/Projet Romaine - Contrat/Chapitres/Chapitre I - Traits d'histoire de vie/Map/alevins_1.csv",header = T, sep = ";")
alevins <- st_as_sf(alevins, coords = c("long", "lat"), crs = 4326)
smolt2<- read.csv("~/Sauvegarde/Projet Romaine - Contrat/Chapitres/Chapitre I - Traits d'histoire de vie/Map/smolt_obj2.csv",header = T, sep = ";")
smolt2 <- st_as_sf(smolt2, coords = c("long", "lat"), crs = 4326)
LARSEM <- read.csv("~/Sauvegarde/Projet Romaine - Contrat/Chapitres/Chapitre I - Traits d'histoire de vie/Map/LARSEM.csv",header = T, sep = ";")
LARSEM <- st_as_sf(LARSEM, coords = c("long", "lat"), crs = 4326)

PU <- subset(watercourses, watercourses$NOMRIV_1=="Rivière Puyjalon")
RO <- subset(watercourses, watercourses$NOMRIV_1=="Rivière Romaine")

#Carte du canada
canada <- ggplot() +
  geom_sf(data = world, fill = "grey")+
  geom_sf(data = watercourses,aes(fill = NOMRIV_1))+
  geom_sf(data = PU, color = "#DC143C")+
  geom_sf(data = RO, color = "#000080", fill = "#000080")+
  geom_sf(data = LARSEM, color = "deeppink",size = 6, pch = 18)+
  coord_sf(xlim = c(-54, -75), c(43, 54), expand = FALSE)+
  scale_fill_manual(values= c("#DC143C","#000080"),labels = c("Puyjalon","Romaine"))+
  geom_rect(aes(
    xmin = -63.9, 
    ymin = 50.2, 
    xmax = -63.4, 
    ymax = 50.5), 
    fill = NA, 
    colour = "black", linewidth = 0.8)+
  theme_classic()+
  theme(panel.border = element_rect(color = "black", fill = NA),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        plot.background = element_blank())+
  annotate(geom = "text", label = "Atlantic\nOcean", x = -56, y =44,size = 2.5)+
  annotate(geom = "text", label = "Canada", x = -70, y =53,size = 2.5)
canada

#Map des échantillonnages

# Legend items (fake layer to generate the legend)
legend_items <- data.frame(
  name = c("header1","Parr","Smolt1","Adults","LARSEM",
           "header2","Fry","Smolt2"),
  lon = -63.7,
  lat = 50.4
)

# Plot
main_map <- ggplot() +
  geom_sf(data = world, color = "grey85", fill = "grey85")+
  geom_sf(data = bv, color = "grey85", fill = "grey85")+
  geom_sf(data = bv2, color= "grey85",fill = "grey85")+
  geom_sf(data = watercourses, aes(fill = NOMRIV_1))+
  #rivers no legend
  geom_sf(data = PU, fill = "#DC143C", color = "#DC143C", alpha = 0.6, show.legend = FALSE) +
  geom_sf(data = RO, fill = "#000080", color = "#000080", alpha = 0.6, show.legend = FALSE) +
  #Sampling sites
  geom_sf(data = parr, aes(fill = "Parr"), size = 5, pch = 24)+
  geom_sf(data = smolt1, aes(fill = "Smolt1"), size = 5, pch = 24)+
  geom_sf(data = mig, aes(fill = "Adults"), size = 5, pch = 24)+
  geom_sf(data = LARSEM, aes(fill = "LARSEM"), size = 6, pch = 23) +
  geom_sf(data = alevins, aes(fill = "Fry"), size = 6, pch = 21)+
  geom_sf(data = smolt2, aes(fill = "Smolt2"), size = 6, pch = 21)+
  # Fake layer to control legend
  geom_point(data = legend_items,
             aes(x = lon, y = lat, fill = name),
             shape = 21, size = 0, alpha = 0) +
  
  coord_sf(xlim = c(-63.85, -63.42), ylim = c(50.28, 50.48), expand = FALSE) +
  
  scale_fill_manual(
    values = c(
      header1 = NA,
      Parr = "orange",
      Smolt1 = "yellow",
      Adults = "forestgreen",
      LARSEM = "deeppink",
      header2 = NA,
      Fry = "pink",
      Smolt2 = "yellow"
    ),
    breaks = legend_items$name,
    labels = c(
      expression(bold("Transferred to the LARSEM")),
      "Parr","Smolt","Adult","LARSEM",
      expression(bold("Natural conditions")),
      "Fry","Smolt"
    )
  ) +
  guides(fill = guide_legend(
    override.aes = list(
      fill = c(NA, "orange", "yellow","forestgreen","deeppink",
               NA, "pink", "yellow"),
      color = c(NA, "orange", "yellow","forestgreen","deeppink",
                NA, "pink", "yellow")
    )
  )) +
  
  # --- Annotate river names directly on map ---
  annotate("text", x = -63.51, y = 50.343, label = "Puyjalon River",
           color = "#DC143C", fontface = "bold", size = 5) +
  annotate("text", x = -63.55, y = 50.297, label = "Romaine River",
           color = "#000080", fontface = "bold", size = 5) +
  
  theme_classic() +
  ggspatial::annotation_scale(location = "tl", bar_cols = c("grey60", "white")) +
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("grey40"),
      line_col = "grey20")
  ) +
  theme(axis.text=element_text(size=15),
        axis.title = element_blank(),
        legend.background = element_rect(color = "black"),
        strip.text.x = element_text(size = 20),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_blank(),
        legend.position = c(0.81,0.835),
        legend.text = element_text(size = 16))

main_map




#Assemblage des deux cartes

map <- ggdraw(main_map) +
  draw_plot(canada,
            x = 0.07,
            y =0.45,
            width = 0.35,
            height =0.35)
map

#Figure 1A







######Analysis_3.2_Genotyping and selection of individuals and markers_________________________________________________________________________

##Genotype are available in the Excel file "Genotype Selection" where we manually removed 
##individuals and loci as explained in the methods.The file in in the Structure folder.

##We used the 333 individuals from the LARSEM to run Structure



######Analysis_3.3_Reference population and population assignment_________________________________________________________________________

# Plot best K from STRUCTURE analysis (SuppMatt2A)
# file path in Results fronm the STRUCTURE analysis

# Have to use the run with all 333 individuals (FirstRun_Structure)

all_files_names_k25 <- list.files(path = "~/Sauvegarde/Projet Romaine - Contrat/Chapitres/Chapitre I - Traits d'histoire de vie/Structure/FirstRun_Structure/PopRef/Results", full.names = TRUE)
all_files_k25 <- vector(mode = "list", length = length(all_files_names_k25))
for(i in seq_along(all_files_k25)){
  all_files_k25[[i]] <- loadStructure(all_files_names_k25[[i]])
}
k_all_k25 <- structList(all_files_k25)

# Evanno
evan_all_k25 <- bestK(k_all_k25, method = "evanno", plot = FALSE)

evan_all_k25 <- subset(evan_all_k25, variable == "delta K")
lk <- subset(evan_all_k25, variable == "L(K)") 
# AIC
aic_all_k25 <- bestK(k_all_k25, method = "structure", plot = FALSE)
aic_all_k25 <- subset(aic_all_k25, variable == "AIC")

aic_all_k25$Low_sd <- aic_all_k25$value - (aic_all_k25$sd/2)
aic_all_k25$High_sd <- aic_all_k25$value + (aic_all_k25$sd/2)

# General plot options
theme_general <- theme(
  panel.background = element_rect(fill = "white", color = "black", size = 1.5),
  plot.background = element_rect(fill = "white", color = NA),
  panel.grid = element_line(color = "gray85", size = 0.75),
  axis.text = element_text(size = 18, color = "black"),
  axis.title = element_text(size = 28),
  legend.text = element_text(size = 25),
  legend.title = element_text(size = 30),
  legend.key = element_rect(fill = "white"),
  legend.key.size = unit(1.2, "cm")
)

# Plot results figure from supplementary materials S2A
plot_clust_all <- ggarrange(
  ggplot(data = evan_all_k25, aes(x = factor(K), y = value)) +
    geom_point(size = 4) +
    labs(x = "K", y = "Delta K") +
    theme_general,
  ggplot(data = aic_all_k25, aes(x = factor(K), y = value)) +
    geom_point(size = 4) +
    geom_errorbar(aes(ymin = Low_sd, ymax = High_sd)) +
    labs(x = "K", y = "AIC") +
    theme_general,
  ncol = 2)
plot_clust_all

### Structure plot (Figure 1B) 

# Select a run for the best K
# Still using the 333 individuals
setwd("~/Sauvegarde/Projet Romaine - Contrat/Chapitres/Chapitre I - Traits d'histoire de vie/Structure/FirstRun_Structure/PopRef/Results")
file_name = "Popref_run_15_f"

# Importing files as lines
file <- readLines(file_name, warn = FALSE)

# Establishing datasets start and end lines 
start_index <- grep("Inferred ancestry of individuals:", file)

end_index <- grep("Estimated Allele Frequencies in each cluster", file) - 3

# Select lines to get the dataset
file <- file[(start_index + 1):end_index]

# Split elements for the second line (first line after columns name) in order to establish how many columns to consider
elements <- unlist(strsplit(file[2], " "))
elements <- elements[elements != ""]

# Create first line, which will become headers
col_names <- c("Label", "Row", "Miss","Pop", "To_remove")
col_names <- c(col_names, paste0("V", seq(from = 1, to = (length(elements) - 5))))

# Add the line (headers) to the dataset
file[1] <- paste(col_names, collapse = " ")

# Convert text into dataframe
file <- read.table(text = file, header = TRUE)

# Clean datasets by removing unwanted rows: Row, Miss and To_remove

file <- file %>%
  dplyr::select(Label, everything(),
         -Row, -Miss, -To_remove) 
#file <- file[,c(1,4,6,7,8,9)]
# Adapt dataset to be plotable

file <- as.data.frame(
  file %>%
    pivot_longer(cols = starts_with("V"), names_to = "Group", values_to = "Probability"))
#Remplacer les pop d'origine
file$Pop <- ifelse(file$Pop == 1,"Romaine","Puyjalon")


#Order

file <- file[order(file$Group,file$Probability),]
file$Label <- factor(file$Label, levels = unique(file$Label))
file$Group <- as.factor(file$Group)
summary(file)

Structure <- ggplot(data = file, aes(x = Label, y = Probability, fill = Group)) +
  geom_bar(stat = "identity", width = 1) +
  facet_grid(
    . ~ Pop,
    drop = TRUE,
    space = "free",
    scales = "free",
    labeller = labeller(Pop = c(
      "Puyjalon" = "River: Puyjalon",
      "Romaine"  = "River: Romaine"
    ))
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(size = 20)
  ) +
  labs(x = "Individuals", y = "Ancestry (%)") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c("#000080", "#DC143C")) +
  theme(
    strip.text.x = element_text(size = 12, face = "bold.italic")
  )


# PCA Figure 1C

setwd("~/Sauvegarde/Projet Romaine - Contrat/Chapitres/Chapitre I - Traits d'histoire de vie/Structure/FirstRun_Structure")
#read.structure to import data set
PopRef41_gen  <- read.structure("STRUCTURE_Filtered_Individuals333_Loci41.stru", 
                                n.ind = 333, n.loc = 41, col.lab = 1, col.pop = 2,col.others = NULL,
                                row.marknames = 1, ask = FALSE)
PopRef41_gen


PCA_data <- tab(PopRef41_gen, freq=TRUE, NA.method="mean")
pca.PopRef41 <- dudi.pca(df = PCA_data, center = TRUE, scale = FALSE, nf = 3, scannf = FALSE)

# Analyse how much percent of genetic variance is explained by each axis
percent = pca.PopRef41$eig/sum(pca.PopRef41$eig)*100
barplot(percent, ylab = "Genetic variance explained by eigenvectors (%)", ylim = c(0,12),
        names.arg = round(percent, 1))

# Create a data.frame containing individual coordinates
ind_coords_FPR41 = as.data.frame(pca.PopRef41$li)
# Rename columns of dataframe
colnames(ind_coords_FPR41) = c("Axis1","Axis2","Axis3")
# Add a column containing individuals
PopRef41 <- read.delim("~/Sauvegarde/Projet Romaine - Contrat/Chapitres/Chapitre I - Traits d'histoire de vie/Structure/FirstRun_Structure/PopRef41.txt")
#ind_coords_FPR41$Ind = indNames(PopRef41_gen)
ind_coords_FPR41$Ind <- PopRef41$ID
# Add a column with the site IDs
ind_coords_FPR41$Site = PopRef41_gen$pop
# Calculate centroid (average) position for each population
centroid = aggregate(cbind(Axis1, Axis2, Axis3) ~ Site, data = ind_coords_FPR41, FUN = mean)
# Add centroid coordinates to ind_coords_FPR41 dataframe
ind_coords_FPR41 = left_join(ind_coords_FPR41, centroid, by = "Site", suffix = c("",".cen"))


# Define colour palette
cols = c("#DC143C","#000080")
# Custom x and y labels
xlab = paste("Axis 1 (", format(round(percent[1], 1), nsmall=1)," %)", sep="")
ylab = paste("Axis 2 (", format(round(percent[2], 1), nsmall=1)," %)", sep="")

# Custom theme for ggplot2
ggtheme = theme(axis.text.y = element_text(colour="black", size=12),
                axis.text.x = element_text(colour="black", size=12),
                axis.title = element_text(colour="black", size=12),
                panel.border = element_rect(colour="black", fill=NA, size=1),
                panel.background = element_blank(),
                legend.position = "bottom")
# Scatter plot axis 1 vs. 2

PCA <- ggplot(data = ind_coords_FPR41, aes(x = Axis1, y = Axis2))+
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  
  # points
  geom_point(aes(fill = Site), shape = 21, size = 3, show.legend = TRUE) +
  
  # labels
  #geom_text(aes(label = ind_coords_FPR41$Ind), hjust = 0, vjust = 0, size = 2) +
  
  # ellipses
  stat_ellipse(aes(color = Site), show.legend = TRUE) +
  
  # legend scales (same name so they merge)
  scale_fill_manual(
    values = cols,
    labels = c("Puyjalon", "Romaine"),
    name = "River"
  ) +
  scale_color_manual(
    values = cols,
    labels = c("Puyjalon", "Romaine"),
    name = "River"
  ) +
  
  # force fill & color to share the legend
  guides(fill = guide_legend(override.aes = list(shape = 21)),
         color = guide_legend(override.aes = list(shape = NA))) +
  
  labs(x = xlab, y = ylab) +
  ggtheme


## Figure 1
map <- map +
  theme(
    plot.margin = margin(0, 0, 0, 0)
  )

map
Structure
PCA

Figure1 <-ggdraw()+
  draw_plot(map, x = 0, y = 0.30, width = 1, height = 0.70)+
  draw_plot(Structure, x = 0, y = 0, width = 0.6, height = 0.3)+
  draw_plot(PCA, x = 0.6, y = 0, width = 0.4, height = 0.3)+
  draw_plot_label(label = c("A", "B","C"), size = 15,
                  x = c(0,0,0.6), y = c(0.97,0.29,0.29))

ggsave(filename = "~/Sauvegarde/Projet Romaine - Contrat/Chapitres/Chapitre I - Traits d'histoire de vie/Resoumission/Figures/Figure1.png",width = 11, height = 12, dpi = 1200)


##In the excel file "Genotype selection" we manually removed the 32 individuals with 
##an ancestry score < 0.85

##The following analysis use the file with only the reference populations.


PopRef41_fin  <- read.structure("STRUCTURE_Filtered_Individuals301_Loci41.stru", 
                                n.ind = 301, n.loc = 41, col.lab = 1, col.pop = 2,col.others = NULL,
                                row.marknames = 1, ask = FALSE)

PopRef41_fin_FST <- genind2hierfstat(PopRef41_fin)
pairwise.neifst(PopRef41_fin_FST)

# Bootstrapping over loci
# Pairwise WC FST
pairwise_WC <- pairwise.WCfst(PopRef41_fin_FST)
pairwise_WC
boot_res <- boot.ppfst(PopRef41_fin_FST, nboot = 1000, quant = c(0.025, 0.975))

boot_res
#Testing null hypothesis
perm_test <- function(dat, nperm = 1000) {
  # Observed Nei's pairwise FST
  obs <- pairwise.neifst(dat)
  
  # Store permuted FST values
  perm_vals <- array(NA, dim = c(dim(obs), nperm))
  
  for (i in 1:nperm) {
    permuted <- dat
    # Shuffle population IDs (assumed in first column)
    permuted[, 1] <- sample(permuted[, 1])
    perm_vals[,, i] <- pairwise.neifst(permuted)
  }
  
  # Calculate adjusted p-values
  pvals <- matrix(NA, nrow = nrow(obs), ncol = ncol(obs))
  for (i in 1:(nrow(obs) - 1)) {
    for (j in (i + 1):ncol(obs)) {
      pvals[i, j] <- (sum(perm_vals[i, j, ] >= obs[i, j]) + 1) / (nperm + 1)
      pvals[j, i] <- pvals[i, j]
    }
  }
  
  list(Fst = obs, pvals = pvals)
}
perm_res <- perm_test(PopRef41_fin_FST, nperm = 1000)
perm_res



#Cross validation Figure in supplementary materials S2B using the file BD_REF_41_loci_301Ind_GENEPOP in population assignment folder
PopRef <- read.Genepop("BD_REF_41_loci_301Ind_GENEPOP.txt", pop.names = c("Romaine","Puyjalon"),haploid = FALSE)
assign.MC(PopRef, train.inds = c(0.5,0.7,0.9), train.loci = c(0.1,0.25,0.5,1),
          loci.sample = "fst", iterations = 1000, model = "svm", dir = "Result/")
# Calulate assignment accuracy
accuMC <- accuracy.MC(dir = "Result/")
accuracy.plot(accuMC, pop = c("all", "Romaine", "Puyjalon"))+
  ylim(0,1)+
  annotate("segment", x = 0.4, xend = 3.6, y =0.5, colour ="red", size = 1)+
  ggtitle("Monte-Carlo cross-validation using genetic loci")+
  theme(plot.title = element_text(size =20, face = "bold"))
ggsave(filename = "~/Th?se/Chapitre I et II/Chapitre I/Figure R/Figure-Structure/Accuracy.png",width = 10, height = 7, dpi = 300)



######Population assignment with Geneclass____________________________________________________________________________________________

##Files are avaible in the folder Population assignment.
##Clean files with only correctly genotyped and assigned individuals were used thereafter,
#available in the folder Code and file.



######Analysis_3.4_3.5 Life history traits in the wild__________________________________________________________________________________________________________
#Use the file wthv in the folder Code and file.
setwd("~/Sauvegarde/Projet Romaine - Contrat/Chapitres/Chapitre I - Traits d'histoire de vie/Code et fichier")
#Importing data set containing all life history traits from the fin-clipped and captured individuals genotyped and assigned to their population
wthv <- read.delim("~/Sauvegarde/Projet Romaine - Contrat/Chapitres/Chapitre I - Traits d'histoire de vie/Code et fichier/wthv.txt")
wthv$Sampling <- as.factor(wthv$Sampling)
wthv$ID <- as.factor(wthv$ID)
wthv$Year <- as.factor(wthv$Year)
wthv$Population <- as.factor(wthv$Population)
wthv$Sexe <- as.factor(wthv$Sexe)
wthv$Assigne <- as.factor(wthv$Assigne)
wthv$Ensemencement <- as.factor(wthv$Ensemencement)
wthv$Origine <- as.factor(wthv$Origine)
wthv$GeCapt <- as.factor(wthv$GeCapt)
wthv$AgeRiv <- as.factor(wthv$AgeRiv)
wthv$LF <- as.numeric(wthv$LF)
wthv$Weight <- as.numeric(wthv$Weight)
wthv$LongueurT <- as.numeric(wthv$LongueurT)
wthv$Environnement <- as.factor(wthv$Environnement)
wthv$Date <- as.Date(wthv$Date)
wthv$Day <- as.Date(wthv$Day)

summary(wthv)

##Analysis for 3.4 --> Fry__________________________________________________________________________________________________________

Alwthv <- subset(wthv, wthv$Sampling == "Alevins")


#Subset for year 2020 with foreign fry

Alwthv2020 <- subset(Alwthv, Alwthv$Year == "2020")

### Impact of environment on Length Figure 4A
strip <- strip_themed(background_x = elem_list_rect(fill = c("#DC143C","#000080")))


bracket_df <- data.frame(
  Population = "Puyjalon",   # le niveau qui correspond au 1er panneau
  xmin = 1,
  xmax = 2,
  y.position = 70,
  label = "***"
)

Env <- ggplot(Alwthv2020) +
  geom_boxplot(aes(x = Environnement, y = LF, colour = Environnement), outlier.shape = NA) +
  geom_jitter(aes(x = Environnement, y = LF, colour = Environnement), alpha = 0.2, width = 0.05) +
  labs(x = "Environment of growth (sampling river)", y = "Fork length (mm)") +
  scale_colour_manual(values = c("grey60","black")) +
  scale_fill_manual(values = c("grey60","black")) +
  facet_wrap2(~Population, scales = "free_x",
              labeller = as_labeller(c("Puyjalon" = "Assign genetically to Puyjalon",
                                       "Romaine" = "Assign genetically to Romaine")),
              strip = strip) +
  scale_x_discrete(labels = c("Puyjalon","Romaine")) +
  geom_bracket(
    data = bracket_df,
    aes(xmin = xmin, xmax = xmax, y.position = y.position, label = label),
    tip.length = 0.01, size = 1, label.size = 6
  ) +
  theme_bw() +
  theme(axis.text = element_text(size = 15, color = "black"),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 15, colour = "white"),
        legend.position = "none")
Env

### Fit of the linear model to see environment impact
alevins_20 <- lm(LF~Population+Environnement, data = Alwthv2020)
summary(alevins_20)
#linearity
plot(alevins_20, which = 1)
#Normality
qqnorm(resid(alevins_20))
qqline(resid(alevins_20), col = "red")
shapiro.test(resid(alevins_20))
#Homoscedasticity
plot(alevins_20, which = 3)
#all
check_model(alevins_20)


# 1. Identify the ROPU rows
ropu_rows <- which(Alwthv2020$GeCapt == "ROPU")
Alwthv2020[ropu_rows, c("ID", "Population", "Environnement", "GeCapt", "LF")]

# 2. Pull leverage (hat values) and Cook's distance for the whole model
infl <- influence.measures(alevins_20)
infl_df <- as.data.frame(infl$infmat)
infl_df$row_id <- 1:nrow(infl_df)
infl_df$GeCapt <- Alwthv2020$GeCapt
infl_df$LF <- Alwthv2020$LF

# 3. Look specifically at the ROPU rows' influence stats
infl_df[infl_df$GeCapt == "ROPU", c("row_id", "GeCapt", "LF", "cook.d", "hat")]

# 4. Compare to the average across all other groups
aggregate(cbind(cook.d, hat) ~ GeCapt, data = infl_df, FUN = mean)

# 5. Common rule-of-thumb cutoffs
p <- length(coef(alevins_20))   # number of parameters (3)
n <- nrow(Alwthv2020)           # 186
cook_cutoff  <- 4 / n            # common Cook's D threshold
hat_cutoff   <- 2 * p / n        # common leverage threshold
cat("Cook's D cutoff:", cook_cutoff, "\n")
cat("Leverage cutoff:", hat_cutoff, "\n")

# 6. Flag which rows exceed thresholds
infl_df[infl_df$cook.d > cook_cutoff | infl_df$hat > hat_cutoff, 
        c("row_id", "GeCapt", "LF", "cook.d", "hat")]

# 7. Sensitivity check: refit without the ROPU rows
alevins_20_noROPU <- lm(LF ~ Population + Environnement, 
                        data = Alwthv2020[-ropu_rows, ])
summary(alevins_20_noROPU)



### Remove foreign fry to compare growth Figure 4B

Alwthv_TRUE <- Alwthv %>% filter(GeCapt %in% c("PUPU", "RORO"))
Alwthv_FALSE <- Alwthv %>% filter(GeCapt %in% c("ROPU", "PURO"))
summary(Alwthv_FALSE)
# Compute means and confidence intervals for each group
summary_data <- Alwthv_TRUE %>%
  group_by(Year, Population) %>%
  summarise(n = n(),
            mean_LF = mean(LF), 
            se = sd(LF) / sqrt(n()),  # Standard Error
            lower = mean_LF - 1.96 * se, 
            upper = mean_LF + 1.96 * se,
            .groups = "drop")


Year <- ggplot(Alwthv_TRUE) +
  geom_boxplot(aes(x = Year, y = LF, colour = Population),
               outlier.shape = NA,
               position = position_dodge(width = 0.75)) +
  geom_jitter(aes(x = Year, y = LF, colour = Population),
              alpha = 0.1,
              position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75)) +
  geom_bracket(xmin = 0.8, xmax = 1.2, y.position = 80, label = "***", tip.length = 0.01, size = 0.5, label.size = 4) +
  geom_bracket(xmin = 2.8, xmax = 3.2, y.position = 80, label = "***", tip.length = 0.01, size = 0.5, label.size = 4) +
  labs(x = "", y = "Fork length (mm)") +
  theme_classic(base_size = 15) +
  scale_colour_manual(values = c("#DC143C","#000080")) +
  scale_fill_manual(values = c("#DC143C","#000080")) +
  coord_cartesian(ylim = c(NA, 85)) +
  theme(axis.text = element_text(size = 15, color = "black"), 
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 18))
Year
ggsave("~/Thèse/Chapitre I et II/Chapitre I/Soumission/Figure/Objectif 2//TailleAlevinsWild.png", width = 9, height = 7, dpi = 900)  

#Fit linear model for not foreign fry
alevins <- lm(LF~Population*Year, data = Alwthv_TRUE)
summary(alevins)

check_model(alevins)
Alwthv_TRUE[which.max(residuals(alevins)), ]

anova(alevins)  

car::Anova(alevins, type = "III")
library(emmeans)
emm <- emmeans(alevins, ~ Population | Year)
pairs(emm)

#Figure 2

Figure2 <-ggdraw()+
  draw_plot(Env, x = 0, y = 0.5, width = 1, height = 0.50)+
  draw_plot(Year, x = 0, y = 0, width = 1, height = 0.5)+
  draw_plot_label(label = c("A", "B"), size = 18,
                  x = c(0,0), y = c(0.97,0.47))

ggsave("~/Sauvegarde/Projet Romaine - Contrat/Chapitres/Chapitre I - Traits d'histoire de vie/Resoumission/Figures/Figure2.png", width = 11, height = 12, dpi = 1200)


##Analysis for 3.5 --> Smolts__________________________________________________________________________________________________________

wthvS <- subset(wthv, wthv$Sampling == "Saumoneaux")

#Timing de dévalaison

wthvS <- wthvS %>%
  mutate(JourJulien = yday(Date))  # jour de l'année (1-365)

#Timing individuel
library(lmerTest)

mod_timing <- lmer(
  JourJulien ~ Population + (1|Year),
  data = wthvS
)

summary(mod_timing)
check_model(mod_timing)


#Durée du run 

disp <- aggregate(JourJulien ~ Population + Year, data = wthvS,
                  FUN = function(x) sd(x, na.rm = TRUE))
disp_wide <- reshape(disp, idvar = "Year", timevar = "Population", direction = "wide")

wilcox.test(disp_wide$JourJulien.Puyjalon, disp_wide$JourJulien.Romaine, paired = TRUE)

disp_wide$diff <- disp_wide$JourJulien.Puyjalon - disp_wide$JourJulien.Romaine
disp_wide

summary_run <- wthvS %>%
  group_by(Population, Year) %>%
  summarise(
    mean_day = mean(JourJulien, na.rm = TRUE),
    sd_day   = sd(JourJulien, na.rm = TRUE),
    .groups = "drop"
  )


RunTiming <- ggplot(summary_run,
                    aes(x = Year,
                        y = mean_day,
                        colour = Population,
                        group = Population)) +
  geom_line(linewidth = 0.8,
            position = position_dodge(width = 0.3)) +
  geom_point(size = 3,
             position = position_dodge(width = 0.3)) +
  geom_errorbar(
    aes(ymin = mean_day - sd_day,
        ymax = mean_day + sd_day),
    width = 0.2,
    linewidth = 0.6,
    position = position_dodge(width = 0.3)
  ) +
  labs(
    x = "",
    y = "Mean migration day (Julian day ± SD)",
    colour = "Population"
  ) +
  scale_colour_manual(values = c("#DC143C", "#000080")) +
  theme_classic(base_size = 15) +
  theme(
    axis.text = element_text(size = 15, colour = "black"),
    axis.title = element_text(size = 18),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 18),
    legend.position = "bottom",
    legend.box.spacing = unit(-15, "pt"))

RunTiming

# Age à dévalaison

#Keeping only aged individuals
wthvS_Measured <- subset(wthvS, wthvS$AgeRiv != "NA")

age <- ggplot(wthvS_Measured, aes(fill=AgeRiv, x=Population)) + 
  geom_bar(position="fill")+
  coord_cartesian(ylim=c(0,1))+
  scale_fill_manual(values = c("palegreen","palegreen3","palegreen4"))+
  theme_classic()+
  theme(axis.text = element_text(size = 15, color = "black"), 
        axis.title = element_blank(),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 18),
        legend.position = "bottom")+
  guides(fill = guide_legend(title = "Migration age"))+
  annotate(geom = "text", x = 1, y = 0.90, label = "25.1%", size = 4)+
  annotate(geom = "text", x = 1, y = 0.40, label = "71.6%", size = 4)+
  annotate(geom = "text", x = 2, y = 0.60, label = "78.2%", size = 4)+
  annotate(geom = "text", x = 2, y = 0.13, label = "20.5%", size = 4)
age
#Chisquare test
# Create the contingency table
age_table <- table(wthvS_Measured$AgeRiv, wthvS_Measured$Population)

# Keep only the rows corresponding to Age 2, 3, and 4
age_table <- age_table[c("2", "3", "4"), ]

# Print the modified table
print(age_table)
#chisq
chi_test <- chisq.test(age_table)
print(chi_test)
print(chi_test$expected)


#Impact des différentes variables sur la longueur à la fourche
#Suppression des 4 ans

df_thresh <- wthvS_Measured %>%
  filter(AgeRiv %in% c("2", "3")) %>%
  mutate(
    Population       = as.factor(Population),
    Year             = as.factor(Year)
  ) %>%
  filter(!is.na(LongueurT))

#Création de la variable Capture

df_thresh <- df_thresh %>%
  group_by(Year) %>%
  mutate(Capture_rel = as.numeric(Day - min(Day))) %>%
  ungroup() %>%
  mutate(Year             = as.factor(Year),
         Population       = as.factor(Population),
         AgeRiv           = as.factor(AgeRiv))

m2  <- lmer(LongueurT ~ Population * AgeRiv +
              Population * Capture_rel +
              AgeRiv * Capture_rel +
              (1 + Population | Year),
            data = df_thresh,REML = FALSE)

# 1. Convergence et singularité
summary(m2)
isSingular(m2)

m2simple  <- lmer(LongueurT ~ Population * AgeRiv +
                    Population * Capture_rel +
                    AgeRiv * Capture_rel +
                    (1 | Year),
                  data = df_thresh,REML = FALSE)

anova(m2simple, m2)

check_model(m2)
plot(fitted(m2), resid(m2), main = "Residuals vs Fitted Values")
abline(h = 0, col = "red", lty = 2)
check_overdispersion(m2)


#Sélection de modèle

# Toutes les interactions doubles
m2  <- lmer(LongueurT ~ Population * AgeRiv +
              Population * Capture_rel +
              AgeRiv * Capture_rel +
              (1 + Population | Year),
            data = df_thresh,REML = FALSE)

# Deux interactions doubles
m3  <- lmer(LongueurT ~ Population * AgeRiv +
              Population * Capture_rel +
              (1 + Population | Year),
            data = df_thresh,REML = FALSE)

m4  <- lmer(LongueurT ~ Population * AgeRiv +
              AgeRiv * Capture_rel +
              (1 + Population | Year),
            data = df_thresh,REML = FALSE)

m5  <- lmer(LongueurT ~ Population * Capture_rel +
              AgeRiv * Capture_rel +
              (1 + Population | Year),
            data = df_thresh,REML = FALSE)

# Une interaction double
m6  <- lmer(LongueurT ~ Population * AgeRiv +
              Capture_rel +
              (1 + Population | Year),
            data = df_thresh,REML = FALSE)

m7  <- lmer(LongueurT ~ Population * Capture_rel +
              AgeRiv +
              (1 + Population | Year),
            data = df_thresh,REML = FALSE)

m8  <- lmer(LongueurT ~ AgeRiv * Capture_rel +
              Population +
              (1 + Population | Year),
            data = df_thresh,REML = FALSE)

# Additifs
m9  <- lmer(LongueurT ~ Population + AgeRiv + Capture_rel +
              (1 + Population | Year),
            data = df_thresh,REML = FALSE)

m10 <- lmer(LongueurT ~ Population + AgeRiv +
              (1 + Population | Year),
            data = df_thresh,REML = FALSE)

m11 <- lmer(LongueurT ~ Population + Capture_rel +
              (1 + Population | Year),
            data = df_thresh,REML = FALSE)

m12 <- lmer(LongueurT ~ AgeRiv + Capture_rel +
              (1 + Population | Year),
            data = df_thresh,REML = FALSE)

# Un seul effet fixe
m13 <- lmer(LongueurT ~ Population +
              (1 + Population | Year),
            data = df_thresh,REML = FALSE)

m14 <- lmer(LongueurT ~ AgeRiv +
              (1 + Population | Year),
            data = df_thresh,REML = FALSE)

m15 <- lmer(LongueurT ~ Capture_rel +
              (1 + Population | Year),
            data = df_thresh,REML = FALSE)

# Modèle nul
m16 <- lmer(LongueurT ~ 1 +
              (1 + Population | Year),
            data = df_thresh,REML = FALSE)

#AIC

library(AICcmodavg)

mods <- list(
  m2, m3, m4, m5, m6, m7, m8,
  m9, m10, m11, m12, m13, m14, m15, m16
)

modnames <- c(
  "All 2-way",
  "Pop×Age + Pop×Date",
  "Pop×Age + Age×Date",
  "Pop×Date + Age×Date",
  "Pop×Age",
  "Pop×Date",
  "Age×Date",
  "Additive",
  "Population + Age",
  "Population + Date",
  "Age + Date",
  "Population",
  "Age",
  "Date",
  "Null"
)

aictab(mods, modnames = modnames)

avg <- model.avg(mods, subset = delta < 2)
summary(avg)

summary(m8)
check_model(m8)
check_normality(m8)
check_heteroscedasticity(m8)
check_singularity(m8)
check_collinearity(m8)
plot(fitted(m8), resid(m8), main = "Residuals vs Fitted Values")
abline(h = 0, col = "red", lty = 2)
check_overdispersion(m8)

sim <- simulateResiduals(m8)
plot(sim)

#Centralisé
df_thresh$Capture_c <- as.numeric(scale(df_thresh$Capture_rel, center = TRUE, scale = FALSE))

centre_capture <- attr(scale(df_thresh$Capture_rel, center = TRUE, scale = FALSE), "scaled:center")
centre_capture

m8 <- lmer(
  LongueurT ~ Population +
    AgeRiv * Capture_c +
    (1 + Population | Year),
  data = df_thresh,
  REML = TRUE
)
summary(m8)
check_model(m8)
library(ggeffects)
library(ggplot2)

# Prédictions du modèle
pred <- ggpredict(
  m8,
  terms = list(
    Capture_c = seq(-15, 30, by = 0.1),
    AgeRiv = unique(df_thresh$AgeRiv),
    Population = unique(df_thresh$Population)
  )
)
summary(pred)
# Retour à l'échelle originale
pred$x <- pred$x + centre_capture

FL <- ggplot(df_thresh,
             aes(x = Capture_rel,
                 y = LongueurT,
                 colour = Population)) +
  
  geom_point(alpha = 0.35, size = 1.8) +
  
  geom_ribbon(data = pred,
              aes(x = x,
                  ymin = conf.low,
                  ymax = conf.high,
                  fill = facet,
                  group = interaction(facet, group)),
              alpha = 0.15,
              colour = NA,
              inherit.aes = FALSE) +
  
  geom_line(data = pred,
            aes(x = x,
                y = predicted,
                colour = facet,
                linetype = group,
                group = interaction(facet, group)),
            linewidth = 1.2,
            inherit.aes = FALSE) +
  
  scale_colour_manual(values = c("Puyjalon" = "#DC143C",
                                 "Romaine" = "#000080")) +
  
  scale_fill_manual(values = c("Puyjalon" = "#DC143C",
                               "Romaine" = "#000080")) +
  
  scale_linetype_manual(values = c("2" = "solid",
                                   "3" = "22"),
                        labels = c("2" = "Age 2",
                                   "3" = "Age 3"),
                        name = "Migration age") +
  
  guides(linetype = guide_legend(override.aes = list(linewidth = 1.3,
                                                     colour = "black")),
         colour = guide_legend(override.aes = list(linewidth = 1.3)),
         fill = "none") +
  
  labs(x = "Relative capture day",
       y = "Fork length (mm)",
       colour = "Population",
       fill = "Population") +
  
  theme_classic(base_size = 16)
FL
# Calculer la différence Age3 - Age2 à plusieurs valeurs de Capture_c, par population

ref_grid <- emmeans(m8, ~ AgeRiv | Population + Capture_c,
                    at = list(Capture_c = seq(-15, 30, by = 5)))
contraste_age <- contrast(ref_grid, method = "revpairwise", by = c("Population", "Capture_c"))
summary(contraste_age, infer = TRUE)  # donne CI et p-value pour chaque point


#Figure 3
Figure3 <-ggdraw()+
  draw_plot(RunTiming, x = 0, y = 0.5, width = 0.5, height = 0.50)+
  draw_plot(age, x = 0.5, y = 0.5, width = 0.5, height = 0.50)+
  draw_plot(FL, x = 0, y = 0, width = 1, height = 0.5)+
  draw_plot_label(label = c("A", "B","C"), size = 18,
                  x = c(0.03,0.49,0.03), y = c(0.97,0.97,0.47))

ggsave("~/Sauvegarde/Projet Romaine - Contrat/Chapitres/Chapitre I - Traits d'histoire de vie/Resoumission/Figures/Figure3.png", width = 11, height = 12, dpi = 1200)



######Analysis_3.6.1_3.6.2 Life history traits in the LARSEM (controlled environment)__________________________________________________________________________________________________________


#Import dataset with registered life history traits

thvlarsa <- read.delim2("~/Sauvegarde/Projet Romaine - Contrat/Chapitres/Chapitre I - Traits d'histoire de vie/Code et fichier/thvlarsa.txt")

thvlarsa$ID<- as.factor(thvlarsa$ID)
thvlarsa$Type <- as.factor(thvlarsa$Type)
thvlarsa$Date <- as.Date(thvlarsa$Date)
thvlarsa$Cohorte<- as.factor(thvlarsa$Cohorte)
thvlarsa$Population<- as.factor(thvlarsa$Population)
thvlarsa$Sexe<- as.factor(thvlarsa$Sexe)
thvlarsa$Repro_1<- as.factor(thvlarsa$Repro_1)
thvlarsa$SexPop <- as.factor(thvlarsa$SexPop)
thvlarsa$logW <- log10(thvlarsa$W)
thvlarsa$logLF <- log10(thvlarsa$LF)
summary(thvlarsa)

##Analysis for 3.6.1_Growth related life-history traits_____________________________________________________________________________________________________________


### Calculation of allometric parameters for log10(W) = b*log10(FL)+log(a)

thvlarsaRO  <- subset(thvlarsa,thvlarsa$Population=="RO")
thvlarsaPU  <- subset(thvlarsa,thvlarsa$Population=="PU")
thvlarsaROF  <- subset(thvlarsaRO,thvlarsaRO$Sexe=="F")
thvlarsaROM  <- subset(thvlarsaRO,thvlarsaRO$Sexe=="M")
thvlarsaPUF  <- subset(thvlarsaPU,thvlarsaPU$Sexe=="F")
thvlarsaPUM  <- subset(thvlarsaPU,thvlarsaPU$Sexe=="M")

modROF <- lm(logW~logLF, data = thvlarsaROF)
summary(modROF)
confint(modROF)

modROM <- lm(logW~logLF, data = thvlarsaROM)
summary(modROM)
confint(modROM)

modPUF <- lm(logW~logLF, data = thvlarsaPUF)
summary(modPUF)
confint(modPUF)

modPUM <- lm(logW~logLF, data = thvlarsaPUM)
summary(modPUM)
confint(modPUM)

library(dplyr)
library(purrr)

# Store models in a named list
models <- list(
  ROF = modROF,
  ROM = modROM,
  PUF = modPUF,
  PUM = modPUM
)

# Function to extract parameters
extract_params <- function(mod) {
  coefs <- coef(mod)
  ci <- confint(mod)
  
  tibble(
    a = exp(coefs[1]),
    a_low = exp(ci[1, 1]),
    a_high = exp(ci[1, 2]),
    b = coefs[2],
    b_low = ci[2, 1],
    b_high = ci[2, 2]
  )
}

# Apply to all models
results <- map_dfr(models, extract_params, .id = "Group")
results <- results %>%
  mutate(across(where(is.numeric), ~ formatC(.x, format = "f", digits = 3)))
results

### Von Bertalanffy model calculation only on smolts

smolt <- subset(thvlarsa, thvlarsa$Type =="Smolt")
smolt <- smolt %>% mutate(Age = (Day / 365))  # Assuming first measurement at ~0 year old
smoltRO <- subset(smolt, smolt$Population =="RO")
smoltROF <- subset(smoltRO, smoltRO$Sexe =="F")
smoltROM <- subset(smoltRO, smoltRO$Sexe =="M")
smoltPU <- subset(smolt, smolt$Population =="PU")
smoltPUF <- subset(smoltPU, smoltPU$Sexe =="F")
smoltPUM <- subset(smoltPU, smoltPU$Sexe =="M")

vbgf_model <- vbFuns(param = "Typical") 
initROF <- vbStarts(LF ~ Age, data = smoltROF)
fit_vbgfROF <- nls(LF ~ vbgf_model(Age, Linf, K, t0),
                   data = smoltROF,
                   start = list(Linf = initROF$Linf, K = initROF$K, t0 = 0),  # Use t0 = starting age
                   control = nls.control(maxiter = 1000, warnOnly = TRUE))
summary(fit_vbgfROF)
initROM <- vbStarts(LF ~ Age, data = smoltROM)
fit_vbgfROM <- nls(LF ~ vbgf_model(Age, Linf, K, t0),
                   data = smoltROM,
                   start = list(Linf = initROM$Linf, K = initROM$K, t0 = 0),  # Use t0 = starting age
                   control = nls.control(maxiter = 1000, warnOnly = TRUE))
summary(fit_vbgfROM)
initPUF <- vbStarts(LF ~ Age, data = smoltPUF)
fit_vbgfPUF <- nls(LF ~ vbgf_model(Age, Linf, K, t0),
                   data = smoltPUF,
                   start = list(Linf = initPUF$Linf, K = initPUF$K, t0 = 0),  # Use t0 = starting age
                   control = nls.control(maxiter = 1000, warnOnly = TRUE))
summary(fit_vbgfPUF)
initPUM <- vbStarts(LF ~ Age, data = smoltPUM)
fit_vbgfPUM <- nls(LF ~ vbgf_model(Age, Linf, K, t0),
                   data = smoltPUM,
                   start = list(Linf = initPUM$Linf, K = initPUM$K, t0 = 0),  # Use t0 = starting age
                   control = nls.control(maxiter = 1000, warnOnly = TRUE))
summary(fit_vbgfPUM)

#Table of all parameters of VBGM and calculation of phi'

# Store models in a named list
models <- list(
  ROF = fit_vbgfROF,
  ROM = fit_vbgfROM,
  PUF = fit_vbgfPUF,
  PUM = fit_vbgfPUM
)

# Function to extract everything
extract_vbgf <- function(mod) {
  s <- summary(mod)
  coefs <- s$coefficients
  
  Linf <- coefs["Linf", "Estimate"]
  SE_Linf <- coefs["Linf", "Std. Error"]
  
  K <- coefs["K", "Estimate"]
  SE_K <- coefs["K", "Std. Error"]
  
  t0 <- coefs["t0", "Estimate"]
  SE_t0 <- coefs["t0", "Std. Error"]
  
  # phi'
  phi_prime <- log10(K) + 2 * log10(Linf)
  
  # SE of phi' (error propagation)
  ln10 <- log(10)
  SE_phi_prime <- sqrt(
    (SE_K / (K * ln10))^2 +
      ((2 * SE_Linf) / (Linf * ln10))^2
  )
  
  tibble(
    Linf = Linf,
    SE_Linf = SE_Linf,
    K = K,
    SE_K = SE_K,
    t0 = t0,
    SE_t0 = SE_t0,
    phi_prime = phi_prime,
    SE_phi_prime = SE_phi_prime
  )
}

# Apply to all models
params <- map_dfr(models, extract_vbgf, .id = "Group")

# Round values
params <- params %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

# View final table
params



###Model construction for the figure 5A
# Apply nlsBoot to the model for bootstrapping
boot_res_ROF <- nlsBoot(fit_vbgfROF, niter = 1000)  # Adjust niter as needed
boot_res_ROM <- nlsBoot(fit_vbgfROM, niter = 1000)
boot_res_PUF <- nlsBoot(fit_vbgfPUF, niter = 1000)
boot_res_PUM <- nlsBoot(fit_vbgfPUM, niter = 1000)


get_bootstrap_preds <- function(boot_res, age_seq) {
  # Extract bootstrapped parameter estimates for Linf, K, and t0
  boot_params <- boot_res$coefboot
  
  # Generate predictions for each bootstrap iteration
  boot_preds <- apply(boot_params, 1, function(params) {
    # Use the parameters to generate predictions for each Age
    Linf <- params[1]
    K <- params[2]
    t0 <- params[3]
    
    # Use the VBGF formula to calculate LF at each age
    LF_pred <- Linf * (1 - exp(-K * (age_seq - t0)))
    return(LF_pred)
  })
  
  # Calculate the confidence intervals (2.5% and 97.5% quantiles)
  ci_lower <- apply(boot_preds, 1, quantile, probs = 0.025)
  ci_upper <- apply(boot_preds, 1, quantile, probs = 0.975)
  
  # Generate the mean prediction (original model prediction)
  #mean_pred <- predict(fit_vbgfROF, newdata = data.frame(Age = age_seq))
  mean_pred <- rowMeans(boot_preds)
  # Return a tibble with the predictions and confidence intervals
  tibble(
    Age = age_seq,
    LF = mean_pred,       # Mean prediction from the model
    LF_LCL = ci_lower,    # Lower 95% CI
    LF_UCL = ci_upper     # Upper 95% CI
  )
}

# Define age sequence for predictions
age_seq <- seq(0, 7, by = 0.01)
# Get predictions and confidence intervals for each model
ci_ROF <- get_bootstrap_preds(boot_res_ROF, age_seq) %>% mutate(Sexe = "F", Population = "RO")
ci_ROM <- get_bootstrap_preds(boot_res_ROM, age_seq) %>% mutate(Sexe = "M", Population = "RO")
ci_PUF <- get_bootstrap_preds(boot_res_PUF, age_seq) %>% mutate(Sexe = "F", Population = "PU")
ci_PUM <- get_bootstrap_preds(boot_res_PUM, age_seq) %>% mutate(Sexe = "M", Population = "PU")


# Combine all CI data
ci_data <- bind_rows(ci_ROF, ci_ROM, ci_PUF, ci_PUM)



# Combine all raw data points
raw_data <- bind_rows(
  smoltROF %>% mutate(Sexe = "F", Population = "RO"),
  smoltROM %>% mutate(Sexe = "M", Population = "RO"),
  smoltPUF %>% mutate(Sexe = "F", Population = "PU"),
  smoltPUM %>% mutate(Sexe = "M", Population = "PU")
)

# Define colors
color_palette <- c("#DC143C", "#FF6347","#000080", "#87CEEB")

# Plot the data with confidence intervals Figure 5A


VBGF <- ggplot() +
  # Confidence intervals using same color aesthetic for fill
  geom_ribbon(data = ci_data, 
              aes(x = Age, ymin = LF_LCL, ymax = LF_UCL, 
                  fill = interaction(Sexe, Population), 
                  color = interaction(Sexe, Population)), 
              alpha = 0.6, color = NA) +  # no ribbon border
  # Raw points
  geom_point(data = raw_data, 
             aes(x = Age, y = LF, color = interaction(Sexe, Population)), 
             alpha = 0.02, size = 1) +
  # Smooth lines
  geom_line(data = ci_data, 
            aes(x = Age, y = LF, color = interaction(Sexe, Population))) +
  # Colors & labels
  scale_color_manual(values = color_palette, 
                     labels = c("F-Puyjalon", "M-Puyjalon", "F-Romaine", "M-Romaine")) +
  scale_fill_manual(values = color_palette, labels = c("F-Puyjalon", "M-Puyjalon", "F-Romaine", "M-Romaine")) +
  labs(x = "Age",
       y = "Fork Length (cm)",
       color = "Group",
       fill  = "Group") +  # same legend title
  theme_classic(base_size = 15) +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
    axis.text = element_text(hjust = 1, size = 15),
    legend.position = c(0.85, 0.4),
    axis.title = element_text(size = 18)
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 3)),
    fill  = guide_legend(override.aes = list(alpha = 0.6))
  )
VBGF
ggsave("~/Thèse/Chapitre I et II/Chapitre I/Soumission/Figure/Objectif 3/VBGF.png", width = 9, height = 7, dpi = 900)

##Analysis for 3.6.2_Fecundity related life-history traits_____________________________________________________________________________________________________________

### L50 for maturation 

#Variable maturation binary, 0 when did not mature yet, 1 when did mature
smolt <- smolt%>%
  group_by(ID)%>%
  mutate(mature = ifelse(cumsum(!is.na(AgeProb)) > 0, 1, 0))

# Identify smolts that matured at least once to calculate mean age at first maturation
smolts_matured <- smolt %>%
  group_by(ID) %>%
  summarise(matured = any(mature == 1)) %>%
  filter(matured == TRUE)

# Identify smolts that never matured
smolts_not_matured <- smolt %>%
  group_by(ID) %>%
  summarise(matured = any(mature == 1)) %>%
  filter(matured == FALSE)

# View summary tables
table_matured <- smolt %>% filter(ID %in% smolts_matured$ID)
table_not_matured <- smolt %>% filter(ID %in% smolts_not_matured$ID)
summary(table_matured)
summary(table_not_matured)

#Avec les individus qui ont maturé (table_matured)

model_full <- glm(
  mature ~ LF * Population * Sexe,
  data = table_matured,
  family = binomial
)
summary(model_full)

model_2way <- glm(
  mature ~ (LF + Population + Sexe)^2,
  family = binomial,
  data = table_matured
)

anova(model_2way, model_full, test = "Chisq")
AIC(model_2way, model_full)
#On garde les interactions

library(DHARMa)

sim <- simulateResiduals(model_full)

plot(sim)

testDispersion(sim)
testOutliers(sim)

#Tableau des coefficient
library(broom)

coef_table <- tidy(model_full,
                   conf.int = TRUE)

coef_table

#Données de prédiction

pred_data <- expand.grid(
  LF = seq(min(table_matured$LF),
           max(table_matured$LF),
           length.out = 1000),
  Population = levels(table_matured$Population),
  Sexe = levels(table_matured$Sexe)
)

pred <- predict(
  model_full,
  newdata = pred_data,
  type = "link",
  se.fit = TRUE
)

pred_df <- pred_data

pred_df$prob  <- plogis(pred$fit)
pred_df$lower <- plogis(pred$fit - 1.96*pred$se.fit)
pred_df$upper <- plogis(pred$fit + 1.96*pred$se.fit)

library(dplyr)

L50 <- pred_df %>%
  group_by(Population, Sexe) %>%
  summarise(
    L50 = approx(
      x = prob,
      y = LF,
      xout = 0.5
    )$y,
    .groups = "drop"
  )

L50


#Bootstrap des L50

library(boot)

boot_L50 <- function(data, indices){
  
  d <- data[indices, ]
  
  fit <- glm(
    mature ~ LF * Population * Sexe,
    family = binomial,
    data = d
  )
  
  nd <- expand.grid(
    LF = seq(min(d$LF),
             max(d$LF),
             length.out = 1000),
    Population = levels(d$Population),
    Sexe = levels(d$Sexe)
  )
  
  nd$prob <- predict(
    fit,
    newdata = nd,
    type = "response"
  )
  
  nd %>%
    group_by(Population, Sexe) %>%
    summarise(
      L50 = approx(
        x = prob,
        y = LF,
        xout = 0.5
      )$y,
      .groups = "drop"
    ) %>%
    arrange(Population, Sexe) %>%
    pull(L50)
  
}

set.seed(123)

boot_res <- boot(
  table_matured,
  statistic = boot_L50,
  R = 1000
)

#IC à 95%

library(purrr)

groupes <- c("PU-F","PU-M","RO-F","RO-M")

L50_boot <- map_dfr(1:4, function(i){
  
  ci <- boot.ci(
    boot_res,
    index = i,
    type = "perc"
  )
  
  tibble(
    Groupe = groupes[i],
    L50 = boot_res$t0[i],
    lower = ci$percent[4],
    upper = ci$percent[5]
  )
  
})

L50_boot

#Graph

L50_plot <- L50_boot %>%
  mutate(
    groupe = c(
      "F.PU",
      "M.PU",
      "F.RO",
      "M.RO"
    )
  )

couleurs <- c(
  "F.PU"="#DC143C",
  "M.PU"="#FF6347",
  "F.RO"="#000080",
  "M.RO"="#87CEEB"
)

etiquettes <- c(
  "F.PU"="Female - Puyjalon",
  "M.PU"="Male - Puyjalon",
  "F.RO"="Female - Romaine",
  "M.RO"="Male - Romaine"
)

library(ggplot2)

p_mature <-
  ggplot(pred_df,
         aes(LF,
             prob,
             colour = interaction(Sexe, Population),
             fill = interaction(Sexe, Population))) +
  
  geom_ribbon(aes(ymin = lower,
                  ymax = upper),
              alpha = 0.2,
              colour = NA) +
  
  geom_line(linewidth = 1.2) +
  
  geom_hline(yintercept = 0.5,
             linetype = 2,
             colour = "grey40") +
  
  geom_vline(data = L50_plot,
             aes(xintercept = L50,
                 colour = groupe),
             linetype = 2,
             linewidth = 0.9) +
  
  scale_colour_manual(values = couleurs,
                      labels = etiquettes) +
  
  scale_fill_manual(values = couleurs,
                    labels = etiquettes) +
  
  labs(x = "Fork length (cm)",
       y = "Probability of maturation") +
  
  theme_classic(base_size = 15) +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
    axis.text = element_text(hjust = 1, size = 15),
    legend.position = c(0.85, 0.35),
    axis.title = element_text(size = 18)
  ) 
p_mature

Figure4 <-ggdraw()+
  draw_plot(VBGF, x = 0, y = 0.5, width = 1, height = 0.50)+
  draw_plot(p_mature, x = 0, y = 0, width = 1, height = 0.5)+
  draw_plot_label(label = c("A", "B"), size = 18,
                  x = c(0,0), y = c(0.97,0.47))

ggsave("~/Sauvegarde/Projet Romaine - Contrat/Chapitres/Chapitre I - Traits d'histoire de vie/Resoumission/Figures/Figure4.png", width = 11, height = 12, dpi = 1200)

### Maturation status

# Seuils de taille correspondant à la borne inférieure du L50 pour chaque groupe
size_threshold <- L50_boot %>%
  separate(Groupe, into = c("Population", "Sexe"), sep = "-") %>%
  select(Population, Sexe, lower)

# Dernière mesure disponible par individu
last_measure <- smolt %>%
  group_by(ID) %>%
  slice_max(Age, n = 1, with_ties = FALSE) %>%
  ungroup()

# Individus à exclure :
# - n'ont jamais maturé
# - leur taille maximale reste sous la borne inférieure du L50 de leur groupe
ids_exclude <- last_measure %>%
  left_join(size_threshold, by = c("Population", "Sexe")) %>%
  filter(LF < lower,
         mature == 0) %>%
  pull(ID)

# Nombre d'individus exclus
length(unique(ids_exclude))

# Garder toutes les mesures des individus non exclus
smolt_filtered2 <- smolt %>%
  filter(!ID %in% ids_exclude)

# Vérification : nombre d'individus avant et après filtrage
tibble(
  Avant = n_distinct(smolt$ID),
  Après = n_distinct(smolt_filtered2$ID),
  Exclus = length(unique(ids_exclude))
)

# ── 1. Résumé par individu ────────────────────────────────────────────────────
maturity_ind <- smolt_filtered2 %>%
  group_by(ID, Sexe, Population) %>%
  summarise(matured = max(mature), .groups = "drop")

# ── 2. Tableau résumé ─────────────────────────────────────────────────────────
tableau_maturity <- maturity_ind %>%
  group_by(Sexe, Population) %>%
  summarise(
    n_total     = n(),
    n_mature    = sum(matured == 1),
    n_non_mature = sum(matured == 0),
    pct_mature  = round(mean(matured == 1) * 100, 1),
    .groups     = "drop"
  )

print(tableau_maturity)

# ── 3. Graphique ──────────────────────────────────────────────────────────────
plot_data <- maturity_ind %>%
  group_by(Sexe, Population) %>%
  summarise(
    Mature     = mean(matured == 1) * 100,
    `Not Mature` = mean(matured == 0) * 100,
    .groups    = "drop"
  ) %>%
  pivot_longer(cols = c("Mature", "Not Mature"),
               names_to  = "Status",
               values_to = "Percentage") %>%
  mutate(
    Status     = factor(Status, levels = c("Not Mature", "Mature")),
    Population = recode(Population, "PU" = "Puyjalon", "RO" = "Romaine")
  )

ggplot(plot_data, aes(x = Sexe, y = Percentage, fill = Status)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Population) +
  scale_fill_manual(values = c("Not Mature" = "#4B2D6E", "Mature" = "#2E7D32")) +
  labs(x    = "Sex",
       y    = "Percentage of Maturation",
       fill = "Maturation Status") +
  theme_classic(base_size = 14) +
  theme(strip.background = element_rect(fill = "grey85", color = NA),
        strip.text       = element_text(size = 13))

# Test de fisher

# Tableau global
fisher.test(matrix(c(224,46,133,3), nrow=2,
                   dimnames=list(Population=c("PU","RO"),
                                 Maturation=c("Mature","Non_mature"))))

# Femelles
fisher.test(matrix(c(147,26,69,2), nrow=2,
                   dimnames=list(Population=c("PU","RO"),
                                 Maturation=c("Mature","Non_mature"))))

# Mâles
fisher.test(matrix(c(77,20,64,1), nrow=2,
                   dimnames=list(Population=c("PU","RO"),
                                 Maturation=c("Mature","Non_mature"))))

### Maturation timing within a year.
#Use the repro file.
repro <- read.delim("F:/Thèse/Projet Romaine - Contrat/Chapitres/Chapitre I - Traits d'histoire de vie/Code et fichier")
summary(repro)
repro$Population <- as.factor(repro$Population)

# Set locale to English for month names (especially if you're in a French system)
Sys.setlocale("LC_TIME", "C")  # or try "en_US.UTF-8" on Unix

# Clean and convert
repro <- repro %>%
  mutate(
    Date_clean = trimws(Date),  # remove any accidental leading/trailing space
    FullDate = as.Date(paste0(Date_clean, "-", Year), format = "%d-%b-%Y"),
    Days = as.numeric(format(FullDate, "%j"))
  )
summary(repro)
# Comparison

ggplot(repro, aes(x = Population, y = Days, fill = Population)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "black") +
  labs(title = "Day of Year at Maturation by Population", y = "Day of Year", x = "") +
  theme_classic()

by(repro$Days, repro$Population, shapiro.test)

leveneTest(Days ~ Population, data = repro)

#Welch t-test
t.test(Days ~ Population, data = repro, var.equal = FALSE)

repro %>%
  group_by(Population) %>%
  summarise(
    n = n(),
    mean_day = mean(Days, na.rm = TRUE),
    sd_day = sd(Days, na.rm = TRUE)
  )

#Calculation of SE
repro %>%
  group_by(Population) %>%
  summarise(
    n = n(),
    mean_day = mean(Days, na.rm = TRUE),
    sd_day = sd(Days, na.rm = TRUE),
    se_day = sd_day / sqrt(n),
    ci_lower = mean_day - 1.96 * se_day,
    ci_upper = mean_day + 1.96 * se_day
  )

##Fecundity related analysis

repro_all <- subset(thvlarsa, thvlarsa$Repro_1!="NA")
repro_all_F <- subset(repro_all, repro_all$Sexe == "F")

#Calculation of relative fecundity and Weggs

repro_all_F$Fec <- repro_all_F$Eggs/repro_all_F$W
repro_all_F$WSegg <- repro_all_F$Wegg/repro_all_F$Eggs

summary(repro_all_F)

# 54 parr matured with only 1 from Puyjalon

#Using only the smolts

repro_all_F_S <- subset(repro_all_F, repro_all_F$Type == "Smolt")

vars <- repro_all_F_S[, c("W", "K", "Dia", "WSegg")]
cor_matrix <- cor(vars, use = "complete.obs")
# Print correlation matrix
print(cor_matrix)

# Visualize correlation matrix
corrplot(cor_matrix, method = "color", tl.cex = 1.2, addCoef.col = "black")
#Dia and WSegg correlate 0.75
#W and K correlate 0.58

repro_clean <- na.omit(repro_all_F_S)

repro_clean$W_c <- as.numeric(scale(repro_clean$W, scale = TRUE))
repro_clean$Dia_c <- as.numeric(scale(repro_clean$Dia, scale = TRUE))


#Mesures répétés et hétéroscédasticité, ou lmm avec effet aléatoire
library(lme4)
library(lmerTest)  # pour avoir les p-values (lmer seul ne les donne pas par défaut)

lmerEgg <- lmer(log(Eggs) ~ (W_c + Dia_c) * Population + (1|ID), data = repro_clean)
summary(lmerEgg)

sim <- simulateResiduals(lmerEgg)
plot(sim)

m_quad <- lmer(log(Eggs) ~ (W_c + I(W_c^2) + Dia_c) * Population +
                 (1 | ID),
               data = repro_clean)
summary(m_quad)
sim2 <- simulateResiduals(m_quad)
plot(sim2)

m_quad_nodia <- lmer(
  log(Eggs) ~ (W_c + I(W_c^2))*Population + Dia_c + (1|ID),
  data = repro_clean
)

anova(
  update(m_quad, REML = FALSE),
  update(m_quad_nodia, REML = FALSE)
)

m_quad_final <- lmer(
  log(Eggs) ~ (W_c + I(W_c^2))*Population +
    Dia_c +
    (1|ID),
  data = repro_clean
)

m_no_quadint <- lmer(
  log(Eggs) ~
    W_c*Population +
    I(W_c^2) +
    Dia_c +
    (1|ID),
  data = repro_clean
)

anova(
  update(m_quad_final, REML = FALSE),
  update(m_no_quadint, REML = FALSE)
)

m_final <- lmer(
  log(Eggs) ~ (W_c + I(W_c^2))*Population +
    Dia_c +
    (1|ID),
  data = repro_clean,
  REML = TRUE
)
summary(m_final)

sim3 <- simulateResiduals(m_final)
plot(sim3)
testOutliers(sim3, plot = TRUE)
testUniformity(sim3)
testDispersion(sim3)

#Check outliers
repro_check <- repro_clean

repro_check$DHARMa_resid <- sim3$scaledResiduals

repro_check <- repro_check[order(abs(repro_check$DHARMa_resid - 0.5),
                                 decreasing = TRUE), ]

repro_check[1:15, c("ID", "Population", "W", "Dia", "Eggs", "DHARMa_resid")]

#Graph
library(lme4)
library(ggplot2)
library(ggeffects)


# Valeurs utilisées pour le centrage
W_mean <- mean(repro_clean$W, na.rm = TRUE)
W_sd   <- sd(repro_clean$W, na.rm = TRUE)

Dia_mean <- mean(repro_clean$Dia, na.rm = TRUE)
Dia_sd   <- sd(repro_clean$Dia, na.rm = TRUE)

# Diamètre fixé à la médiane
Dia_median <- median(repro_clean$Dia, na.rm = TRUE)

# Prédictions du modèle
pred <- ggpredict(
  m_final,
  terms = c("W_c [all]", "Population"),
  condition = list(
    Dia_c = (Dia_median - Dia_mean) / Dia_sd
  )
)

# Retour à l'échelle originale du poids
pred$x <- pred$x * W_sd + W_mean

# Vérification rapide
head(pred)
summary(pred$predicted)

#Figure

library(ggnewscale)

oeuf <- ggplot(pred, aes(x = x, y = predicted)) +
  
  # Courbes prédites (dessinées en dessous)
  geom_line(
    aes(colour = group),
    linewidth = 1.2
  ) +
  
  scale_colour_manual(
    values = c("PU" = "#DC143C", "RO" = "#000080"),
    guide = "none"
  ) +
  
  # On réinitialise l'échelle de couleur pour les points
  new_scale_colour() +
  
  # Intervalle de confiance (par-dessus la ligne, avec transparence)
  geom_ribbon(
    aes(
      ymin = conf.low,
      ymax = conf.high,
      fill = group
    ),
    alpha = 0.4
  ) +
  
  # Données observées
  geom_point(
    data = repro_clean,
    aes(
      x = W,
      y = Eggs,
      colour = Dia
    ),
    alpha = 0.8,
    size = 3
  ) +
  
  scale_colour_viridis_c(
    name = "Egg diameter"
  ) +
  
  scale_fill_manual(
    values = c(
      "PU" = "#DC143C",
      "RO" = "#000080"
    ),
    labels = c(
      "PU" = "Puyjalon",
      "RO" = "Romaine"
    ),
    name = "Population"
  ) +
  
  guides(
    fill = guide_legend(order = 1),
    colour = guide_colourbar(order = 2)
  ) +
  
  labs(
    x = "Weight (g)",
    y = "Fecundity (Number of eggs)"
  ) +
  
  theme_classic(base_size = 15) +
  theme(
    legend.text = element_text(size = 15),
    axis.text = element_text(hjust = 1, size = 15),
    axis.title = element_text(size = 18)
  )
oeuf
### Comparison between Romaine parr and smolts

repro_all_F_T <- subset(repro_all_F, repro_all_F$Population == "RO")
repro_clean_T <- na.omit(repro_all_F_T)
summary(repro_clean_T)

repro_clean_T$W_c <- as.numeric(scale(repro_clean_T$W, scale = TRUE))
repro_clean_T$Dia_c <- as.numeric(scale(repro_clean_T$Dia, scale = TRUE))

m_quad <- lmer(log(Eggs) ~ (W_c + I(W_c^2) + Dia_c) * Type +
                 (1 | ID),
               data = repro_clean_T)
summary(m_quad)
sim2 <- simulateResiduals(m_quad)
plot(sim2)

m_quad_nodia <- lmer(
  log(Eggs) ~ (W_c + I(W_c^2))*Type + Dia_c + (1|ID),
  data = repro_clean_T
)

anova(
  update(m_quad, REML = FALSE),
  update(m_quad_nodia, REML = FALSE)
)

m_quad_final <- lmer(
  log(Eggs) ~ (W_c + I(W_c^2))*Type +
    Dia_c +
    (1|ID),
  data = repro_clean_T
)
summary(m_quad_final)

m_no_quadint <- lmer(
  log(Eggs) ~
    W_c*Type +
    I(W_c^2) +
    Dia_c +
    (1|ID),
  data = repro_clean_T
)

anova(
  update(m_quad_final, REML = FALSE),
  update(m_no_quadint, REML = FALSE)
)

m_final <- lmer(
  log(Eggs) ~ (W_c + I(W_c^2))*Type +
    Dia_c +
    (1|ID),
  data = repro_clean_T,
  REML = TRUE
)
summary(m_final)

sim3 <- simulateResiduals(m_final)
plot(sim3)
testOutliers(sim3, plot = TRUE)
testUniformity(sim3)
testDispersion(sim3)

#Graph
library(lme4)
library(ggplot2)
library(ggeffects)


# Valeurs utilisées pour le centrage
W_mean <- mean(repro_clean_T$W, na.rm = TRUE)
W_sd   <- sd(repro_clean_T$W, na.rm = TRUE)

Dia_mean <- mean(repro_clean_T$Dia, na.rm = TRUE)
Dia_sd   <- sd(repro_clean_T$Dia, na.rm = TRUE)

# Diamètre fixé à la médiane
Dia_median <- median(repro_clean_T$Dia, na.rm = TRUE)

# Prédictions du modèle
pred <- ggpredict(
  m_final,
  terms = c("W_c [all]", "Type"),
  condition = list(
    Dia_c = (Dia_median - Dia_mean) / Dia_sd
  )
)

# Retour à l'échelle originale du poids
pred$x <- pred$x * W_sd + W_mean

# Vérification rapide
head(pred)
summary(pred$predicted)

#Figure
pred <- pred %>%
  dplyr::filter(!(group == "Tacon" & x > 9610))

oeuf_T <- ggplot(pred, aes(x = x, y = predicted)) +
  
  # Courbes prédites (dessinées en dessous)
  geom_line(
    aes(colour = group),
    linewidth = 1.2
  ) +
  
  scale_colour_manual(
    values = c("Smolt" = "#492050", "Tacon" = "#2C792D"),
    labels = c("Smolt" = "Smolt", "Tacon" = "Parr"),
    guide = "none"
  ) +
  
  # On réinitialise l'échelle de couleur pour les points
  new_scale_colour() +
  
  # Intervalle de confiance (par-dessus la ligne, avec transparence)
  geom_ribbon(
    aes(
      ymin = conf.low,
      ymax = conf.high,
      fill = group
    ),
    alpha = 0.4
  ) +
  
  # Données observées
  geom_point(
    data = repro_clean_T,
    aes(
      x = W,
      y = Eggs,
      colour = Dia
    ),
    alpha = 0.8,
    size = 3
  ) +
  
  scale_colour_viridis_c(
    name = "Egg diameter"
  ) +
  
  scale_fill_manual(
    values = c(
      "Smolt" = "#492050", 
      "Tacon" = "#2C792D"
    ),
    labels = c("Smolt" = "Smolt", "Tacon" = "Parr"),
    name = "Stage"
  ) +
  
  guides(
    fill = guide_legend(order = 1),
    colour = guide_colourbar(order = 2)
  ) +
  
  labs(
    x = "Weight (g)",
    y = "Fecundity (Number of egg)"
  ) +
  
  theme_classic(base_size = 15) +
  theme(
    legend.text = element_text(size = 15),
    axis.text = element_text(hjust = 1, size = 15),
    axis.title = element_text(size = 18)
  )
oeuf_T

Figure5 <-ggdraw()+
  draw_plot(oeuf, x = 0, y = 0.5, width = 1, height = 0.50)+
  draw_plot(oeuf_T, x = 0, y = 0, width = 1, height = 0.5)+
  draw_plot_label(label = c("A", "B"), size = 18,
                  x = c(0,0), y = c(0.97,0.47))

ggsave("~/Sauvegarde/Projet Romaine - Contrat/Chapitres/Chapitre I - Traits d'histoire de vie/Resoumission/Figures/Figure5.png", width = 11, height = 12, dpi = 1200)

