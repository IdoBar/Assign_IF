remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"), INSTALL_opts = "--no-multiarch")

pacman::p_load(tidyverse, janitor, tabulizer)


# library(rvest) # to parse the abbreviation table from the web
# content <- read_html("https://woodward.library.ubc.ca/research-help/journal-abbreviations/")
# 
# tables <- content[[2]] %>% html_table(fill = TRUE)
# 
# first_table <- tables[[1]]

# Read IF table from PDF
# IF_table <- extract_tables("data/JCR-2021.pdf")
# 
# headers <- stringi::stri_remove_empty(IF_table[[1]][1,]) %>% 
#   snakecase::to_any_case("snake")
# first_page <- apply(IF_table[[1]][-1,], 1, 
#                     stringi::stri_remove_empty) %>% 
#   do.call(rbind, .) %>% as_tibble() %>% 
#   setNames(headers)
# complete_IF_table <- IF_table[2:length(IF_table)] %>% 
#   map_dfr(as_tibble) %>% setNames(headers) %>%  
#   bind_rows(first_page, .)  %>% 
#   filter(!rank==total_cites, 
#           journal_impact_factor!="Not Available") %>% 
#   janitor::remove_empty(which = "rows") %>% 
#   mutate(journal_IF=as.numeric(journal_impact_factor),
#          full_title=snakecase::to_any_case(full_journal_title, "title")) %>% write_csv("data/complete_IF_table.csv")
complete_IF_table <- read_csv("data/complete_IF_table.csv") %>% 
  filter(!is.na(journal_impact_factor))

my_IF_table <- tribble(~full_title, ~journal_IF,
                       "Microbial Genomics", 5.237, 
                       "Genes", 4.096,
                       "Scientific Reports", 4.380,
                       "SpringerPlus", 1.130,
                       "Frontiers in Chemistry", 5.221,
                       "Frontiers in Plant Science", 5.754,
                       "Frontiers in Genetics", 4.599
                       )

# load abbreviation table
abbr_table <- read_delim("data/wos_abbrev_table.csv", delim = ";", 
                         quote = '"') %>% 
  mutate(full_title=snakecase::to_any_case(full, "title"))
# create final csv table
final_table <- complete_IF_table %>% 
  select(full_title, journal_IF) %>%
  bind_rows(my_IF_table) %>% 
  right_join(abbr_table %>% 
             select(full_title, abbrev), .) %>% 
  # select(full, abbrev, journal_impact_factor) %>%
  write_csv("data/ZotIF_data.csv", col_names = FALSE)

