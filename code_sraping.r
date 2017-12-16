# Practicing scraping web 
# Data available from https://www.analyticsvidhya.com/blog/2017/03/beginners-guide-on-web-scraping-in-r-using-rvest-with-hands-on-knowledge/

# Junsok Huhh anarinsk@gmail.com
# first made in 12-14-2017

# Initialize ----
#install.packages('rvest')
library('rvest')
library('tidyverse')
library('stringr')
library('wrapr')
library('here')

# Scrap IGM US topic url ----
url <- 'http://www.igmchicago.org/igm-economic-experts-panel'
webpage <- read_html(url)
#Using CSS selectors to scrap the rankings section
survey_list_html <- html_nodes(webpage,'h2 a')
cherrypick_html <- function(.){
  
  str_remove1 <- "<a href=\""
  str_remove2 <- "(\">.+</a>)"
  
  str_replace_all(., str_remove1, "") %.>% 
  str_replace_all(., str_remove2, "") %.>% 
  str_trim(.)
  
  }
survey_list_html %.>% map(.,cherrypick_html) %.>% unlist(.) -> survey_list
tibble( list = survey_list, id = seq_len(length(list)) ) -> df_survey

# MAKE DF for answer ----

make_df4issue <- function(uid){
  
  df_survey %.>% slice(., uid) %.>% .[['list']] -> url 
  df_survey %.>% slice(., uid) %.>% .[['id']] -> id
  
  read_html(url) -> thisurl
  # Extract participants 
  cherrypick_partc <- function(x){
  str_extract(x, "participants/.+\"") %.>% 
  str_replace(., "participants/", "") %.>% 
  str_replace(., "-", " ") %.>% 
  str_replace(., "\"", "")
}
  thisurl %.>% 
    html_nodes(., ".response-name a") %.>% 
    map(., cherrypick_partc) %.>% unlist(.) -> col_partc
  # Extract univs
  cherrypick_univ <- function(x){str_replace_all(x, "</td>|<td>", "")} 
  thisurl %.>% 
    html_nodes(., ".response-name+ td") %.>% 
    map(., cherrypick_univ) %.>% unlist(.) -> col_univ
  # Extract votes 
  cherrypick_vote <- function(x){
  str_replace_all(x, "<span class=\"option-.\">|<span class=\"option-\">|\t</span>|\n|\t", "") %.>% 
  .[. %in% c('Strongly Agree', 'Agree', 'Uncertain', 
             'Disagree', 'Strongly Disagree','No Opinion', 
             'Did Not Answer', 'Did not answer', 'Did Not Vote')] %.>% 
  return(.)
  }
  thisurl%.>% 
    html_nodes(., "span") %.>% 
    map(., cherrypick_vote) %.>% unlist(.) -> col_vote
  # Extract confidence 
  cherrypick_confd <- function(x){str_replace_all(x, "<td class=.confCell..|</td>", "")}
  thisurl %.>% 
    html_nodes(., ".confCell") %.>%  
    map(., cherrypick_confd) %.>% unlist(.) -> col_confd
  # Extract comment 
  cherrypick_comment <- function(x){
  str_replace_all(x, "</div>", "") %.>% 
  str_replace_all(., "\n", "") %.>% 
  str_replace_all(., "<div class=\"gridComment", "") %.>% 
  str_replace_all(., "\">", "")
  }
  thisurl %.>% 
    html_nodes(., ".gridComment") %.>% 
    map(., cherrypick_comment) %.>% unlist(.) -> col_comment
  # Make tibble 
  tibble(participant = col_partc, univ = col_univ, 
         vote = col_vote, confident = col_confd, comment = col_comment, id = id) -> dff0
  
  dff0 %.>% filter(., !str_detect(comment, "hvjhv")) -> dff0
  
  dff0 %.>% count(., participant) %.>% nrow(.) -> n_partc 
  nrow(dff0) / n_partc -> n_rep
  unit_rep <- function(x, q){rep(x, q) %.>% tibble(question=.)}
  seq_len(n_rep) %.>% map_df(., function(x){unit_rep(x, n_partc)}) -> dff2
  
  bind_cols(dff0, dff2) %.>% return(.)
  
}

1:4 %.>% map_df(., make_df4issue) -> vdf0
6:157 %.>% map_df(., make_df4issue) -> vdf1
bind_rows(vdf0, vdf1) -> df_answer

#### Make DF for issue, questions, date ----
extract_question <- function(df){
  str_remove1 <- "<h2>|</h2>";
  str_remove2 <- "<h3 class=\"surveyQuestion\">|\n|</h3>";
  str_remove3 <- "<h6>|</h6>";
  str_remove4 <- "(<a href=\".+\">)|</a>" 
  str_remove5 <- "\n|<p>|</p>|</h3>"
  
  df %.>% 
    str_detect(., "This panel explores") %.>% 
    which(.) -> tv 
  
  tv - 1 -> line_cut; 
  
  df[1:line_cut] %.>% 
    str_replace_all(., str_remove2, "") %.>% 
    str_replace_all(., str_remove5, "") %.>% 
    str_c(., collapse = "") %.>% 
    str_split(., "Question..\\:") %.>% 
    unlist(.) -> dff
  
  dff[!dff == ""] %.>% str_trim(.) %.>% return(.) 
} 
make_df4issue_idx <- function(url){
  read_html(url) -> this_url 
  
  str_remove1 <- "<h2>|</h2>";
  str_remove2 <- "<h3 class=\"surveyQuestion\">|\n|</h3>";
  str_remove3 <- "<h6>|</h6>";
  str_remove4 <- "(<a href=\".+\">)|</a>" 
  str_remove5 <- "\n|<p>|</p>|</h3>"
  
  this_url %.>% 
    html_nodes(., "h2") %.>% 
    str_replace_all(., str_remove1, " ") %.>% 
    str_trim(.) -> col_subject 
  
  this_url  %.>% 
    html_nodes(., "p , .surveyQuestion") %.>% 
    extract_question(.) -> col_question
  
  this_url %.>% 
    html_nodes(., "h6") %.>% 
    str_replace_all(., str_remove3, "") %.>% 
    str_trim(.) -> col_time

tibble(subject = col_subject, 
       question = col_question, 
       time = col_time) %.>% return(.)
}

df_survey %.>% slice(., 1:4) %.>% .[['list']] %.>% map_df(., make_df4issue_idx) -> vdf0
df_survey %.>% slice(., 6:157) %.>% .[['list']] %.>% map_df(., make_df4issue_idx) -> vdf1
bind_rows(vdf0, vdf1) -> df_idx 

df_idx %.>% 
  distinct(., subject, time) %.>% 
  mutate(., 
         id = seq_len(nrow(.))
         ) -> tdf0
  
df_idx %.>% group_by(., subject, time) %.>% 
  mutate(., n_q = seq_len(length(subject))) %.>% 
  left_join(., tdf0, by = c('subject', 'time')) -> df_idx

# Clean DFs ----
!ls() %in% c('df_answer', 'df_idx', 'df_survey') -> list_rm 
rm(list=ls()[list_rm])
save.image(here("analysis_IGM","df_src2.RData"))

# End of code 

