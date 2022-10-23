
fails = c("ABAEnrichment", "ASpediaFI", "Autotuner", "BiGGR", "CancerInSilico", 
"CeTF", "ChemmineOB", "coexnet", "coMET", "CountClust", "DEGreport", 
"epihet", "EWCE", "flowCL", "GeneTonic", "GenoGAM", "GenVisR", 
"gpart", "gprege", "Herper", "IsoGeneGUI", "isomiRs", "MACPET", 
"MethCP", "NBSplice", "networkBMA", "Onassis", "orthogene", "perturbatr", 
"PrecisionTrialDrawer", "ProteomicsAnnotationHubData", "pulsedSilac", 
"Rgin", "RMassBank", "RmiR", "RNASeqR", "rsbml", "ScISI", "scRecover", 
"SLGI", "sojourner", "TDARACNE", "tofsims", "TraRe", "Travel", 
"TSRchitect", "XCIR")


BiocManager::install(fails, dependencies=TRUE)
