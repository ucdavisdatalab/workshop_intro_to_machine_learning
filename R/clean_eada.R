
library("readr")
library("stringr")
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

