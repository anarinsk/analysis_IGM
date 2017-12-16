# 12.16. 2017
# Junsok Huhh
#

# Munge df_answer ----

df_answer %.>% 
  mutate(., 
         item = str_c("q_",id, "_", question, sep = ""),
         name = str_to_title(participant)) -> tdf  

# Making testing df 
#tdf %.>% filter(., name %in% c("Daron Acemoglu")) -> tdf0
#tdf %.>% filter(., name %in% c("Daron Acemoglu", "Alberto Alesina")) -> tdf1

make_ordinal_feature <- function(df){
  df %.>% 
    select(., one_of('name', 'item', 'vote')) %.>% 
    spread(., key = item, value = vote) %.>% return(.)
  #-> dff 
  #bind_rows(resdf, dff) ->> resdf
}  
tdf %.>% make_ordinal_feature(.) -> df_answer_wide

here("analysis_IGM", "df_munged.RData") %.>% save.image(.)

#### ++++++++++++++++++++ -----
#### Sidebar & Experimental -----
#### ++++++++++++++++++++ -----

#### Testing complete ----
complete(tdf, item) -> vdf 
