
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

divide_coed = function(df){
  col_names = names(df)
  sports_col = find_subsets(sport, col_names)
  sports_df = df[sports_col]
  
  no_participation = sports_df[!str_detect(names(sports_df), "Participation")]
  Coeds = no_participation[str_detect(names(no_participation), "Coed")]
  
  mens_list = gsub("Coed Team", "Coed Men Team", names(Coeds))
  mens_df = Coeds
  colnames(mens_df) = mens_list
  womens_list = gsub("Coed Team", "Coed Women Team", names(Coeds))
  womens_df = Coeds
  colnames(womens_df) = womens_list
  
  New_Coeds = cbind(mens_df, womens_df)
  No_coed = setdiff(names(df), names(Coeds))
  
  final_df = cbind(df[No_coed], New_Coeds)
  
  return(final_df)
}


pivot_df = function(df, sport_name){
  col_names = colnames(df)
  sport = extract_sports(col_names)
  
  sports_col = find_subsets(sport, col_names)
  base_col = setdiff(col_names, sports_col)
  
  final_df = divide_coed(df)
  
  filtered_sports_df = final_df[str_detect(colnames(final_df), sport_name)]
  sport_df_col = colnames(filtered_sports_df)
  
  pattern = paste0("^", sport_name, " (Men|Women|Coed|Total)(.*)")
  name_check = str_match(sport_df_col, pattern)[,1]
  sport_df_col = sport_df_col[which(!is.na(name_check))]
  
  df_filtered = final_df[c(sport_df_col, base_col)]
  
  total_participation_index = which(str_detect(colnames(df_filtered), paste0(sport_name, " Total Participation"))) #filter out No Participants
  df_filtered = df_filtered[!is.na(df_filtered[[total_participation_index]]), ]
  
  new_cols = str_replace_all(sport_df_col, c("Men's" = "Men", "Women's" = "Women", "Total" = "Total Team")) #change col names to be uniform
  all_new_cols = c(new_cols, setdiff(colnames(df_filtered), sport_df_col)) 
  colnames(df_filtered) = all_new_cols
  
  colnames(df_filtered) = str_replace_all(colnames(df_filtered), "Coed Team Men", "Coed Men Team")
  colnames(df_filtered) = str_replace_all(colnames(df_filtered), "Coed Team Women", "Coed Women Team")
  
  participation_pattern = "Team Participation" 
  revenue_pattern = "Team Revenue" 
  oe_pattern = "Team Operating Expenses" 
  te_pattern = "Team Expense" 
  
  participation_cols = colnames(df_filtered)[str_detect(colnames(df_filtered), participation_pattern)]
  participation_cols = participation_cols[str_detect(participation_cols, sport_name)]
  
  revenue_cols = colnames(df_filtered)[str_detect(colnames(df_filtered), revenue_pattern)]
  revenue_cols = revenue_cols[str_detect(revenue_cols, sport_name)]
  
  oe_cols = colnames(df_filtered)[str_detect(colnames(df_filtered), oe_pattern)]
  oe_cols = oe_cols[str_detect(oe_cols, sport_name)]
  
  te_cols = colnames(df_filtered)[str_detect(colnames(df_filtered), te_pattern) & !str_detect(colnames(df_filtered), oe_pattern)]
  te_cols = te_cols[str_detect(te_cols, sport_name)]
  
  
  pivoted_filtered = df_filtered %>%
    pivot_longer(
      cols = all_of(c(participation_cols, revenue_cols, oe_cols, te_cols)),
      names_to = c("Sports Team", ".value"),
      names_pattern = paste0("(.*Team) (Participation|Revenue|Operating Expenses|Expenses)")
    )
  
  pivoted_filtered = pivoted_filtered %>%
    filter(!is.na(Participation))
  
  pivoted_filtered = as.data.frame(pivoted_filtered)
  
  
  return(pivoted_filtered)
  
}


