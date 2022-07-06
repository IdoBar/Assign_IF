devtools::source_gist("7f63547158ecdbacf31b54a58af0d1cc", filename = "util.R")
# remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer", "dgrtwo/fuzzyjoin"), INSTALL_opts = "--no-multiarch")

pacman::p_load(tidyverse, readxl, snakecase, janitor) # , tabulizer, fuzzyjoin

# See details in https://github.com/peumartino/ZotIF
# Abbreviation tables from https://su.figshare.com/articles/dataset/Journal_abbreviations_from_Web_of_Science/3207787
# IF CJR data downloaded from https://phdtalks.org/2021/05/download-journal-impact-factor-list-2021.html or https://phdtalks.org/2021/05/download-scie-journal-list-latest.html or https://impactfactorforjournal.com/jcr-2021/


# library(rvest) # to parse the abbreviation table from the web
# content <- read_html("https://woodward.library.ubc.ca/research-help/journal-abbreviations/")
# 
# tables <- content[[2]] %>% html_table(fill = TRUE)
# 
# first_table <- tables[[1]]

journal_abvs <- c("CA", "ICES", "BMC", "ACTA", "AAPG", "AAPS", "AATCC", "ACM", "AAS", "ACI", "AEU", "AERA", "AI", "AIAA", "AIBR", "AIDS", "AIMS", "ASTIN",
                  "AJAR", "AJIDD", "AJIL", "AKCE", "ALEA", "AMB", "AMA", "ALTEX", "ATW", "PDE", "ANZ", "AoB", "AORN", "APL", "APMIS", "ARDEA", "ARS", "ASAIO",
                  "ASCE", "ASME", "ASHRAE", "AStA", "ATLA", "AUK", "BJS", "BJU", "BMB", "BMGN", "BMJ", "BRQ", "BSGF", "CBE", "COMPEL", "COPD", "CMC",
                  "CUAJ", "CPT","CTS", "DARU", "ENT", "ESAIM", "EURE", "GAIA", "GLQ", "HAHR", "HAU", "ICHNOS", "ICON", "IEEE", "IRAL", "ISJ", "ITEA",
                  "JAAPA", "JACC", "JAIDS", "JANAC", "JARO", "JARQ", "JASSS", "JAVMA", "JAVNOST", "JCMS", "JCPSP", "JCR", "JNCI", "JNP", "JOGNN", "JPAD", 
                  "JPC","JSLS","KGK","LIBRI", "LUTS", "LWT", "MAPAN", "MCN", "MMWR", "NAUNYN", "NJAS", "NODEA", "NWIG", "ORL", "OTJR", "PFG", "QJM", "PS",
                  "QME", "RAE", "RAIRO", "RBGN", "REAL", "REDIA", "REVSTAT", "RIDE", "RILCE", "RLA", "ROFO", "RSF", "SADHANA", "SEN",  "SHILAP", "SIAM",
                  "TOPIA", "TRAC" , "TRAMES", "UHOD", "VIAL", "ZAMM", "ZDM"
                  )
# Parse IF table from PDF ####
# IF_table <- extract_tables("data/JCR-2021.pdf")
# 
# headers <- stringi::stri_remove_empty(IF_table[[1]][1,]) %>%
#   to_snake_case()
# first_page <- apply(IF_table[[1]][-1,], 1,
#                     stringi::stri_remove_empty) %>%
#   do.call(rbind, .) %>% as_tibble(.name_repair = "unique") %>%
#   setNames(headers)
# complete_IF_table <- IF_table[2:length(IF_table)] %>%
#   map_dfr(~as_tibble(.x, .name_repair = "unique")) %>% setNames(headers) %>%
#   bind_rows(first_page, .)  %>%
#   filter(!rank==total_cites,
#           journal_impact_factor!="Not Available") %>%
#   remove_empty(which = "rows") %>%
#   distinct() %>%
#   mutate(journal_IF=as.numeric(journal_impact_factor),
#          full_title=to_title_case(gsub("&", "and", full_journal_title, fixed = TRUE), abbreviations = journal_abvs, parsing_option=3)) %>%
#   write_csv("data/complete_IF_table.csv")

# Parse IF table from Excel ####
complete_IF_table <- readxl::read_excel("data/JCR2021.xlsx", sheet = "Table_clean") %>% 
  clean_names() %>% 
    # filter(journal_impact_factor!="Not Available") %>%
    remove_empty(which = "rows") %>%
    distinct() %>%
    mutate(full_title=to_title_case(gsub("&", "and", full_journal_title, fixed = TRUE), 
                                    abbreviations = journal_abvs, parsing_option=3)) %>% 
  write_csv("data/JCR2021_IF_table.csv")

complete_IF_table <- read_csv("data/complete_IF_table.csv") %>%
  filter(!is.na(journal_impact_factor))

#

# abbr <- complete_IF_table %>% select(full_journal_title) %>% 
#   filter(grepl("^[A-Z]+\\-", full_journal_title)) %>% 
#   mutate(abbr=str_extract(full_journal_title, pattern = "^[A-Z]+\\-"),
#          abbr=sub("-", "", abbr, fixed = TRUE)) %>% distinct(abbr) %>% arrange(abbr) 

# manually add IF data for journals not found in the database from https://jcr.clarivate.com/jcr/browse-journals
my_IF_table <- tribble(~full_title, ~journal_IF, # ~Publisher,
                       "Microbial Genomics", 5.237, #"Microbiology Society"
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
  mutate(full_title=to_title_case(gsub("&", "and", full, fixed = TRUE), abbreviations = journal_abvs, parsing_option=3))
# create final csv table
final_table <- complete_IF_table %>% 
  select(full_title, journal_IF) %>%
  bind_rows(my_IF_table) %>% 
  right_join(abbr_table %>% 
             select(full_title, abbrev), .) %>% 
  distinct() %>% 
  # select(full, abbrev, journal_impact_factor) %>%
  write_csv("data/ZotIF_data.csv", col_names = FALSE)

# find a free journal 
# data downloaded from tables of journals with ACP agreement from https://www.griffith.edu.au/library/research-publishing/open-research/read-and-publish-agreements (open the links to each publisher and then the journal tables)
free_agreement_files <- "data/CAUL_agreement_all_publishers.csv"# list.files("data", "_agreement.csv", full.names = TRUE)
# free_agreement_files[4] %>% map_dfr(.f = read_csv) %>% 
#   mutate(full_title=snakecase::to_any_case(Journal, "title")) %>%
#   clean_names("mixed") %>% left_join(final_table)


free_journals <- free_agreement_files %>% map_dfr(.f = read_csv) %>% 
  clean_names("mixed") %>% 
  mutate(full_title=gsub("Thalassas:.+", "Thalassas", Journal),
         full_title=to_title_case(gsub("&", "and", full_title, fixed = TRUE), abbreviations = journal_abvs, parsing_option=3)) %>%
  left_join(final_table %>% select(-abbrev))
  # stringdist_left_join(final_table %>% select(-abbrev), max_dist=1.75)

free_journals %>% filter(is.na(journal_IF))

search_terms <- c("molecular", "genomics", "marine", "evolution", "coral")
re_string <- paste(search_terms, collapse = "|")
# find journals 
marine_mol_gen_journals <- free_journals %>% filter(grepl(re_string, Journal, ignore.case = TRUE),
                         Included_in_R_P_Agreement=="Yes") %>% 
  remove_empty() %>% distinct() %>% 
  arrange(desc(journal_IF)) %>% print(n=Inf)

write_csv(marine_mol_gen_journals, "data/marine_molecular_free_journals.csv")

# marine_mol_gen_journals %>% filter(grepl("Thalassas|Marine Ecology", full_title)) %>% select(Journal, full_title) %>% 
#   stringdist_left_join(final_table %>% select(-abbrev) %>% filter(grepl("Thalassas|Marine Ecology", full_title)) , method = "cosine", distance_col = "dist", max_dist= 30)
#   
#   final_table %>% filter(grepl("Journal of Marine Science", full_title, ignore.case = T))
  
# create search string at https://www.resurchify.com/find/?query=Marine+Ecology  
  

