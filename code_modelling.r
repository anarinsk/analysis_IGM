# 12.16. 2017
# Junsok Huhh
#

# Load basic packages and RData
library('tidyverse')
library('wrapr')
library('here')
#install.packages(c("FactoMineR", "factoextra"))
#library("FactoMineR")
#library("factoextra")
#install.packages('klaR')

here('analysis_IGM', 'df_munged.RData') %.>% load(.)

# Clustering by k-modes ----

library('klaR')
# Change name of clustering data 
df_answer_wide4 -> tdf
as.data.frame(tdf) -> tdf 
row.names(tdf) <- tdf$name
tdf[, -c(1)] -> tdf
tdf[is.na(tdf)] <- 3

set.seed(12315)
kmodes(tdf, 2, iter.max = 10, weighted = FALSE) -> clu_res 

fviz_cluster(list(data = tdf, cluster = clu_res$clu))

df_answer_wide4$kmodes <- clu_res$clu
df_answer_wide4 %.>% count(., kmodes)
df_answer_wide4 %.>% dplyr::select(., one_of('name', 'kmodes')) -> zdf5

tfunc <- function(x){median(x, na.rm = TRUE)}

df_answer_wide4[, -c(1)] %.>% 
  group_by(., kmodes) %.>% 
  summarise_all(., tfunc) -> zdf0

zdf0 %.>% 
  gather(., key = "q", value = "median", -c(clu)) %.>% 
  spread(., key = clu, value = median) -> zdf1

zdf1 %.>% 
  rename(.,
    q_id = q, clu1 = '1', clu2 = '2'
  ) %.>% 
  mutate(.,
    dff = clu1 - clu2
  ) -> zdf2

zdf2 %.>% count(., dff)

zdf2 %.>% filter(., abs(dff) >= 1) -> zdf3
zdf3 %.>% count(., clu1)
zdf3 %.>% count(., clu2)
zdf2 %.>% filter(., dff %in% c(-1, 1.5)) -> zdf4

# H clustering ----

# Change name of clustering data 
change_rownames <- function(df, this_names){
  df %.>% as.data.frame(.) -> tdf 
  row.names(tdf) <- this_names
  
  tdf %.>% return(.)
}
be_ready <- function(df){
  change_rownames(df, df$name) %.>% .[, -c(1)] -> tdf
  tdf[is.na(tdf)] <- 3
  
  tdf %.>% return(.)
  }

library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms

my_dendplot <- function(my_dend, my_k = 3){
  my_dend %.>% 
    as.dendrogram(.) -> my_dend2
  
  my_dend2 %.>% 
  color_branches(., k = my_k) %.>% 
  set(., "labels_cex", 0.6) %.>% 
  raise.dendrogram (., 2) %.>% 
  hang.dendrogram(., hang_height = 5) %.>% 
  plot(., horiz=TRUE)
  # add horiz rect
  my_dend2 %.>% rect.dendrogram(., k = my_k, horiz = TRUE, border = 8)
  # add horiz (well, vertical) line:
  
  abline(v = heights_per_k.dendrogram(my_dend2)[as.character(my_k)] + 5, lwd = 2, lty = 2, col = "blue")
}

# Compute with agnes & diana 

be_ready(df_answer_wide4) -> tdf 

# Agglomerative coefficient
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")
m %.>% map_dbl(., function(x){agnes(tdf, method = x) %.>% .$ac })
rm(m)

# Do AGNES 
agnes_opt <- agnes(tdf, method = "ward")
agnes_opt %.>% my_dendplot(., 3) 

# Diagnostic for AGNES 
fviz_nbclust(tdf, FUN = hcut, method = "wss")
fviz_nbclust(tdf, FUN = hcut, method = "silhouette")
gap_stat <- clusGap(tdf, FUN = hcut, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

# Cut tree into 3 groups
sub_agnes <- cutree(as.hclust(agnes_opt), k = 3)
# Number of members in each cluster
fviz_cluster(list(data = tdf, cluster = sub_agnes))

# compute divisive hierarchical clustering
diana <- diana(tdf)
diana %.>% my_dendplot(., 3)
sub_diana <- cutree(as.hclust(diana), k = 3)
fviz_cluster(list(data = tdf, cluster = sub_diana))

# Descrptive Analysis ----

df_answer_wide4 -> df_desc
df_desc$agnes <- sub_agnes

my_med <- function(x){median(x, na.rm = TRUE)}
make_group_med <- function(NA_2_value = FALSE, df = df_desc){
  if (NA_2_value == FALSE) { 
    df %.>% dplyr::select(., -one_of('name')) -> tdf } else {
    df %.>% dplyr::select(., -one_of('name')) -> tdf; tdf[is.na(tdf)] <- 3; }
  
  tdf %.>% 
    group_by(., agnes) %.>% 
    summarise_all(., my_med) %.>% return(.)
    gather(., key = "q", value = "median", -c(agnes)) %.>% 
    spread(., key = agnes, value = median) %.>% 
    rename(., q_id = q, clu1 = '1', clu2 = '2', clu3 = '3' ) %.>% return(.) 
  }
count_group_med <- function(df){
  df %.>% count(., clu1) %.>% rename(., gmed = clu1, n_g1 =n) -> zdf1 
  df %.>% count(., clu2) %.>% rename(., gmed = clu2, n_g2 =n) -> zdf2 
  df %.>% count(., clu3) %.>% rename(., gmed = clu3, n_g3 =n) -> zdf3 
  zdf1 %.>% 
    full_join(., zdf2, by = c('gmed')) %.>% 
    full_join(., zdf3, by = c('gmed')) %.>% 
    arrange(., gmed) %.>% return(.) }

make_group_med(FALSE) %.>% count_group_med(.) 
make_group_med(TRUE) %.>% count_group_med(.) 

make_group_med(TRUE) -> zdf1

zdf1 %.>% 
  mutate(.,
    dff_12 = clu1 - clu2,
    dff_23 = clu2 - clu3,
    dff_13 = clu1 - clu3 
  ) -> zdf2

count_group_dff <- function(df = df_desc){
  make_group_med(TRUE) %.>% 
    mutate(.,
           dff_12 = clu1 - clu2,
           dff_23 = clu2 - clu3,
           dff_13 = clu1 - clu3 
    ) -> zdf2
  
  zdf2 %.>% count(., dff_12) %.>% rename(., g_dff = dff_12, n_12 = n) -> zdf12
  zdf2 %.>% count(., dff_13) %.>% rename(., g_dff = dff_13, n_13 = n) -> zdf13
  zdf2 %.>% count(., dff_23) %.>% rename(., g_dff = dff_23, n_23 = n) -> zdf23
  
  zdf12 %.>% 
    full_join(., zdf13, by = c('g_dff')) %.>% 
    full_join(., zdf23, by = c('g_dff')) %.>% return(.)
}

count_group_dff()

zdf2 %.>% 
  filter(., abs(dff_12) >= 2 | abs(dff_23) >=2 | abs(dff_13) >= 2) -> zdf3

zdf2 %.>% 
  filter(., clu1 == 1 & dff_12 <= -3 | clu1 == 1 & dff_13 <= -3 |
            clu1 == 2 & dff_12 <= -2 | clu1 == 2 & dff_13 <= -2 |
            clu2 == 1 & dff_12 >=  3 | clu2 == 1 & dff_23 <= -3 |
            clu2 == 2 & dff_12 >=  2 | clu2 == 2 & dff_23 <= -2 |
            clu3 == 1 & dff_13 >=  3 | clu3 == 1 & dff_23 >=  3 |
            clu3 == 2 & dff_13 >=  2 | clu3 == 2 & dff_23 >=  2 
           ) -> zdf4
