This GitHub repository provides all datasets and R code for the analyses in the article "Contrasting life history traits in two genetically distinct Atlantic salmon (Salmo salar) populations from the same watershed" by Louarn Fauchet et al. (2026).
All analyses are run from a single R script (in the Code and files folder). The script is organized according to the sections of the Materials and Methods and is commented throughout. File paths in the script must be adapted to your local copy of the repository.
Repository structure :
Map: shapefiles (watercourses and watershed boundaries) and sampling locations (`.csv`) used to draw the map in Figure 1A.
Structure: STRUCTURE runs and genotype data.
`Genotype Selection.xlsx`: microsatellite genotypes, with the individuals and loci removed during filtering (section 3.2).
`FirstRun_Structure`: STRUCTURE run on the 333 LARSEM individuals and 41 loci, used to determine the best K (Supplementary Materials S8), for the STRUCTURE plot (Figure 1B) and for the PCA (Figure 1C).
`FinalRun_Structure`: run on the 301 individuals of the reference populations (ancestry coefficient ≥ 0.85), used to compute pairwise F_ST.
Population assignment: GENEPOP file of the reference populations (used for the assignpop cross-validation, Supplementary Materials S8) and GeneClass input/output files used to assign all other individuals to the Romaine or Puyjalon population (section 3.3).
Code and files: the R script and the cleaned datasets (genotyped and assigned individuals only) used for life-history trait analyses.
`wthv.txt`: life-history traits of fry and smolts in the natural environment (sections 3.4 and 3.5; Figures 2 and 3).
`thvlarsa.txt`: growth, maturation and fecundity data of individuals reared in controlled conditions at the LARSEM (section 3.6; Figures 4 and 5).
`repro.txt`: maturation timing data (section 3.6.2).
Related work
Fauchet et al. (2026). Multidisciplinary Evaluation of a 10-Year Restoration Program for Two Endangered Atlantic Salmon (Salmo salar) Populations. Evolutionary Applications. Data and code: https://github.com/louarnfauchet-hub/salmosalar_Romaine_stocking
