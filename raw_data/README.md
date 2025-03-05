Raw data files for OYTB and QGBCC Rockfish complex species analysis.

OYTB workflow:

1) wt.txt is the most recent data download from the Microsoft Access Database.
  -used to create a Recruitment2024.csv table (data_prep/legacy_recruitment_table.R)

2) props_sfla contains settlement rates for the Black and Yellowtail Rockfish (OYTB complex) and must be merged with the Recruitment2024.csv file created using the legacy code_recruitment_table_CR.R. 
This is done using the data_prep/OYTB_recruitment_table_CR.R script to create species_recruitment_CR.R file that can then be used in the species_specific_settlement_plots.R file for species specific settlement rates/plots.

QGBC workflow:

1) Open ../data_prep/README.md to find instructions on how to dowload, analyze, and assign ID to samples.
2) Download sequencing data from University of Oregon with the following command:  
    wget -r -c -nH --cut-dirs=1 --no-parent --reject "index.html*" https://gc3fstorage.uoregon.edu/LPTBD/8636/
3) Move through ../data_prep/QGBCC to create SAM files, convert them to BAM, and alignment stats from each project
4) Move through ../analysis/QGBCC in Rstudio for the assignment analysis to generate the final list of samples and their IDs.
5) wt.txt is the most recent data download from the Microsoft Access Database.
  -used to create a Recruitment2024.csv table (data_prep/legacy_recruitment_table.R)
6) Use Recruitment2024.csv table in the species specific_settlement_plots.R to create figures for settlement of the QGBCC Rockfish complex
