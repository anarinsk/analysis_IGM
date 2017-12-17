# 12.16. 2017
# Junsok Huhh
#

# Load packages and RData ----
library('tidyverse')
library('stringr')
library('forcats')
library('here')
library('wrapr')

here("analysis_IGM", "df_src2.RData") -> load_RData
load(load_RData)

# Munge df_answer ----

#c("Strongly Disagree", "Disagree", "Uncertain", "Agree", "Strongly Agree") -> answer_levels
c("Strongly Agree", "Agree", "Uncertain", "Disagree", "Strongly Disagree") -> answer_levels

df_answer %.>% 
  mutate(., 
         item = str_c("q_", id, "_", question, sep = ""),
         name = str_to_title(participant)) -> tdf  

tdf %.>% 
  mutate(., 
         vote2 = case_when(
           vote %in% c("Did not answer", "Did Not Answer", "Did Not Vote", "No Opinion") ~ "NA", 
           TRUE ~ vote )
  ) %.>% 
  mutate(., 
         vote2 = ifelse(vote2 == "NA", NA, vote2),
         vote3 = parse_factor(vote2, answer_levels),
         vote4 = as.integer(vote3), 
         vote4 = ifelse(vote4 == 6, NA, vote4)
  )-> tdf

# Making testing df 
#tdf %.>% filter(., name %in% c("Daron Acemoglu")) -> tdf0
#tdf %.>% filter(., name %in% c("Daron Acemoglu", "Alberto Alesina")) -> tdf1

make_ordinal_feature <- function(df, what_vote="vote3"){
  let(
    c(THIS_VOTE = what_vote),
    df %.>% 
    dplyr::select(., one_of('name', 'item', what_vote)) %.>% 
    spread(., key = item, value = THIS_VOTE) %.>% return(.)
  )
}  
tdf %.>% make_ordinal_feature(., "vote")  -> df_answer_wide1
tdf %.>% make_ordinal_feature(., "vote2") -> df_answer_wide2
tdf %.>% make_ordinal_feature(., "vote3") -> df_answer_wide3
tdf %.>% make_ordinal_feature(., "vote4") -> df_answer_wide4

rm(tdf)
here("analysis_IGM", "df_munged.RData") %.>% save.image(.)

#### ++++++++++++++++++++ -----
#### Sidebar & Experimental -----
#### ++++++++++++++++++++ -----

#### Testing complete ----
complete(tdf, item) -> vdf 
