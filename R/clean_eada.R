
library("dplyr")
library("readr")
library("stringr")
library("tidyverse")
library("purrr")


main = function(
  path = "data/equity_in_athletics/2003-2022_equity-in-athletics.csv"
) {
  eada = read_csv(path)
  #names(eada) = 
  #  str_fix_names(names(eada))
  names = names(eada)
  sports = extract_sports(names)
  pattern = str_glue("^({str_flatten(sports, '|')})")
  names[!str_detect(names, pattern)]
  return(eada)
}


str_fix_names = function(names) {
  names = str_replace_all(names, "\\s+", "_")
  names = str_to_lower(names)
  names
}

extract_sports = function(names) {
  sports = str_match(
    names, "(\\w+( \\w+)*) (Men's|Women's|Coed|Total)(.*)")[, 2]
  sports = setdiff(
    sports, c("Grand", "Total", "Grand Total", "Unduplicated Count"))
  sports[!is.na(sports)]
}

find_subsets = function(list1, list2) {
  result = c()
  for (item1 in list1) {
    for (item2 in list2) {
      if (str_detect(item2, fixed(item1))) {
        result = c(result, item2)
      }
    }
  }
  unique(result)
}

pivot_df = function(df, sport_name){
  
  #find columns that contain specific sport and for Survey Year and Institution Name
  matching_columns = str_detect(colnames(df), paste(c(sport_name, "Survey Year", "Institution Name"), collapse = "|"))
  df_filtered = df[matching_columns]
  
  total_participation_index = which(str_detect(colnames(df_filtered), "Total Participation")) #filter out No Participants
  df_filtered = df_filtered[!is.na(df_filtered[[total_participation_index]]), ]
  
  colnames(df_filtered) = str_replace_all(colnames(df_filtered), "Coed Team Men", "Coed Men Team")
  colnames(df_filtered) = str_replace_all(colnames(df_filtered), "Coed Team Women", "Coed Women Team")
  
  new_cols = str_replace_all(colnames(df_filtered), c("Men's" = "Men", "Women's" = "Women", "Total" = "Total Team"))
  colnames(df_filtered) = new_cols
  
  participation_pattern = "Team Participation" 
  revenue_pattern = "Team Revenue" 
  oe_pattern = "Team Operating Expenses" 
  te_pattern = "Team Expense" 
  
  participation_cols = colnames(df_filtered)[str_detect(colnames(df_filtered), participation_pattern)]
  revenue_cols = colnames(df_filtered)[str_detect(colnames(df_filtered), revenue_pattern)]
  oe_cols = colnames(df_filtered)[str_detect(colnames(df_filtered), oe_pattern)]
  te_cols = colnames(df_filtered)[str_detect(colnames(df_filtered), te_pattern) 
                                  & !str_detect(colnames(df_filtered), oe_pattern)]
  
  pivoted_filtered = df_filtered %>%
    pivot_longer(
      cols = all_of(c(participation_cols, revenue_cols, oe_cols, te_cols)),
      names_to = c("Sports Team", ".value"),
      names_pattern = paste0("(.*Team) (Participation|Revenue|Operating Expenses|Expenses)")
    )
  
  pivoted_filtered = pivoted_filtered %>%
    filter(!is.na(Participation))
  
  pivoted_filtered[is.na(pivoted_filtered)] = 0
  
  
  return(pivoted_filtered)
}
