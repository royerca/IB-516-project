### Microhaplot

######################################
### INSTALL PACKAGES & LOAD FUNCTIONS

#packages_needed <- c("devtools", "rlang","ggplot2", "tidyverse","dplyr", "remotes", "pbkrtest", "car", "rstatix", "lme4", "nloptr", "ggpubr")

# for(i in 1:length(packages_needed)){
#   if(!(packages_needed[i] %in% installed.packages())){install.packages(packages_needed[i], repos = "http://cran.us.r-project.org")}
#   library(packages_needed[i], character.only = TRUE)
# }

#devtools::install_github("ngthomas/microhaplot", build_vignettes = TRUE, build_opts = c("--no-resave-data", "--no-manual"))


#################################################################################

#################################################################################
##microhaplot needs a tab delimited label file with the sam file name, sample name, and group label

### Make label.txt file
library(tidyverse)
library(magrittr)


##make a dataframe of the file names for the sam alignment files##
##put the path to the sam files for the list.files command##

samp_names<-as.data.frame(list.files("C:/Users/olsenk2/Shiny/microhaplot/extdata/SAM"))

##make column name easier to call##
colnames(samp_names)<-"sam_file"


##create ID column and remove last 8 characters from sam file name

samp_names %<>%
  mutate(ID = substr(sam_file, 1, nchar(sam_file)-8)) %>%
  mutate(group_label = "sebastes_spp")
  
##write label file##

write.table(samp_names, quote = FALSE, col.names = FALSE, row.names = FALSE, sep = "\t", file = "C:/Users/olsenk2/Shiny/microhaplot/extdata/sebastes_microhap_labels.txt")

#################################################################################

#################################################################################
# provide a directory path to host the microhaplot Shiny app
microhaplot::mvShinyHaplot("C:/Users/olsenk2/Shiny")
library(microhaplot)
app.path <- "C:/Users/olsenk2/Shiny/microhaplot"
run.label <- "sebastes"
sam.path <- "C:/Users/olsenk2/Shiny/microhaplot/extdata/SAM"


label.path <- "C:/Users/olsenk2/Shiny/microhaplot/extdata/sebastes_microhap_labels.txt"
vcf.path <- "C:/Users/olsenk2/Shiny/microhaplot/extdata/sebastes_sppID_combined_filtered.recode.vcf"


haplo.read.tbl <- prepHaplotFiles(run.label = run.label,
                                  sam.path = sam.path,
                                  label.path = label.path,
                                  out.path = tempdir(),
                                  vcf.path = vcf.path,
                                  app.path = app.path,
)

runShinyHaplot("C:/Users/olsenk2/Shiny/microhaplot")
