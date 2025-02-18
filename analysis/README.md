The analysis subdirectory includes R scripts for:
  - SMURF legacy settlement plots (Daniel Ottmann, 2016)
      - uses data_prep/legacy code_recruitment_table_CR.R
          - input = ../raw_data/wt.txt
          - output = Recruitment2024.csv
  - Species specific settlement plots (OYTB and QGBCC)
      - uses data_prep/OYTB recruitment_table_CR.R
          - input = Recruitment2024.csv
          - output = OYTB_Recruitment2024.csv
  - 2021 ODFW synthesis report figures
          - input = Recruitment2024.csv
          - output = ../final_figs/synthesis_report_figs

QGBC analysis scripts coming soon
