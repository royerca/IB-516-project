library(magrittr)
library(tidyverse)
library(rubias)

##analysis of .rds output file from microhapot##

path<-"C:/Users/olsenk2/Shiny/microhaplot/sebastes.rds"

call_genos_from_haplotRDS <- function(path, min_depth1 = 10, min_depth2 = 6, min_balance = 0.4) {
  
  rds <- readRDS(path) %>%
    tbl_df() %>%
    select(-sum.Phred.C, -max.Phred.C, -group) %>%
    filter(rank <= 2) %>%
    arrange(id, locus, rank) %>%
    filter(allele.balance >= min_balance)      # filter on allele balance.
  
  
  # now, some stuff to toss all individual x locus combos that
  # have a rank2 or a rank1 read depth of less than 10.  So, I don't want to
  # call it if has 10 reads of one allele and 3 of another.  Better to just leave
  # it uncalled, I think
  rds2 <- rds %>%
    group_by(id, locus) %>%
    filter((n()==2 && depth[2] < min_depth2) + (n() == 1 && depth[1] < min_depth1) == 0) %>%
    ungroup()
  
  # now we want to fill that out so everyone has a rank2 row, but it is explicitly NA for our homozygotes.
  # I should be able to do this with tidyr::complete, but that is always baffling to me.
  rds3 <- expand.grid(id = unique(rds$id), locus = unique(rds$locus), rank = 1:2, stringsAsFactors = FALSE) %>%
    tbl_df() %>%
    arrange(id, locus, rank) %>%
    left_join(., rds2)
  
  # and now we assign gene_copy1 and gene_copy2
  rds4 <- rds3 %>%
    group_by(id, locus) %>%
    mutate(allele = ifelse(is.na(haplo), haplo[1], haplo),
           depth = ifelse(is.na(depth), depth[1], depth)) %>%
    rename(gene_copy = rank) %>%
    select(id, locus, gene_copy, allele, depth, allele.balance) %>%
    ungroup()
  
  rds4
}


dir <- "C:/Users/olsenk2/Shiny/microhaplot"

genos_long <- call_genos_from_haplotRDS(path = file.path(dir, "sebastes.rds"))


genos_long_explicit_NAs <- genos_long %>%
  select(id) %>%
  unique() %>%
  unlist() %>%
  unname() %>%
  expand.grid(id = ., locus = unique(genos_long$locus), gene_copy = 1:2, stringsAsFactors = FALSE) %>%
  tbl_df() %>% 
  left_join(., genos_long) %>%
  arrange(id, locus, gene_copy)


genos_long_explicit_NAs %>%
  group_by(id) %>%
  tally()


ind_to_toss <- genos_long_explicit_NAs %>%
  group_by(id) %>%
  filter(is.na(allele)) %>% # missing data
  tally() %>%
  arrange(desc(n)) %>% # remove samples with >20% missing data
  filter(n > 36) 


genos_ind_filtered <- genos_long_explicit_NAs %>%
  anti_join(., ind_to_toss)


##load the baseline reference microhaplotypes for each species##

baseline <- readRDS("C:/Users/olsenk2/OneDrive - Oregon State University/Desktop/Rockfish Microhaplotype/sebastes_spp_id_baseline_haplotypes.rds")

#remove 6 markers in the baseline that were removed from the panel and not genotyped in the samples##

markers_in_baseline_not_in_genos<-which(! baseline$locus %in% genos_ind_filtered$locus)

baseline90<-baseline[-markers_in_baseline_not_in_genos, ]

baseline90 %>%
  select(collection) %>%
  unique() %>%
  arrange()

tossers <- baseline90 %>%
  select(indiv, gtseq_run, id) %>%
  unique() %>%
  group_by(indiv) %>%
  tally() %>%
  filter(n >1)

baseline90_one_each <- baseline90 %>%
  anti_join(., tossers)

baseline_spp_info <- baseline90_one_each %>%
  select(sample_type, repunit, collection, indiv, gtseq_run, id, species) %>%
  unique()
baseline_spp_info$gtseq_run <- as.character(baseline_spp_info$gtseq_run)


for_alleidx <- baseline90_one_each %>%
  select(-indiv, -c(1:3, 12:13), -species)


for_alleidx$gtseq_run <- as.character(for_alleidx$gtseq_run)

merged_df <- bind_rows(for_alleidx, genos_ind_filtered)

alle_idxs <- merged_df %>% 
  dplyr::select(gtseq_run, id, locus, gene_copy, allele) %>%
  group_by(locus) %>%
  mutate(alleidx = as.integer(factor(allele, levels = unique(allele)))) %>%
  ungroup() %>%
  arrange(gtseq_run, id, locus, alleidx) # rubias can handle NA's, so no need to change them to 0's

two_col <- alle_idxs %>%
  #group_by(indiv, locus) %>%
  unite(loc, locus, gene_copy, sep = ".") %>%
  #ungroup() %>%
  select(-allele) %>%
  pivot_wider(names_from = loc, values_from = alleidx) 

reference <- two_col %>%
  left_join(., baseline_spp_info) %>%
  filter(!is.na(species)) %>%
  select(-gtseq_run, -id, -species) %>%
  select(sample_type, repunit, collection, indiv, everything())

rubias_mix <- two_col %>%
  anti_join(., baseline_spp_info) %>%
  select(-gtseq_run) %>%
  mutate(sample_type = "mixture", collection = "blackspotted_rougheye", repunit = NA) %>%
  select(sample_type, repunit, collection, everything()) %>%
  rename(indiv = id)


##run rubias

rubias_output <- infer_mixture(reference = reference, mixture = rubias_mix, gen_start_col = 5)

rubias_output$mixing_proportions %>%
  ggplot(aes(x = collection, y = pi)) +
  geom_bar(stat = "identity") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0.95)
  )


top_assign <- rubias_output$indiv_posteriors %>%
  group_by(indiv) %>%
  slice_max(., order_by = PofZ)


top_assign 

top_assign %>%
  filter(z_score < -3 | z_score > 3)


top_assign %>%
  group_by(collection) %>%
  tally()
