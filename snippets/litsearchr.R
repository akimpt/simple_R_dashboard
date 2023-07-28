library(litsearchr)

library(tidyverse)

library(wosr)

library(websearchr)

library(PRISMAstatement)

# remember to open exported RIS in mendeley and then export again to fix fields

records_identified_through_database_searching <- import_results(file="WoS.ris", verbose = TRUE)

records_after_duplicates_removed <-
  litsearchr::remove_duplicates(records_identified_through_database_searching, field = "title", method = "string_osa") %>% 
  mutate(exclude = FALSE)

n_records_after_duplicates_removed = nrow(records_after_duplicates_removed)

write.csv(records_after_duplicates_removed, "./records_after_duplicates_removed.csv") 

rakedkeywords <-
  litsearchr::extract_terms(
    text = paste(records_after_duplicates_removed$title, records_after_duplicates_removed$abstract),
    method = "fakerake",
    min_freq = 2,
    ngrams = TRUE,
    min_n = 2,
    language = "English"
  )
#> Loading required namespace: stopwords

all_keywords <- unique(rakedkeywords)

naivedfm <-
  litsearchr::create_dfm(
    elements = paste(records_after_duplicates_removed$title, records_after_duplicates_removed$abstract),
    features = all_keywords
  )

naivegraph <-
  litsearchr::create_network(
    search_dfm = naivedfm,
    min_studies = 2,
    min_occ = 2
  )

plot(naivegraph)

cutoff <-
  litsearchr::find_cutoff(
    naivegraph,
    method = "cumulative",
    percent = .80,
    imp_method = "strength"
  )

reducedgraph <-
  litsearchr::reduce_graph(naivegraph, cutoff_strength = cutoff[1])

searchterms <- litsearchr::get_keywords(reducedgraph)

head(searchterms, 20)

write.csv(searchterms, "./search_terms.csv")

# update to pull from an edited version where the student ticks boxes for relevance

mysearchterms <- mysearchterms <-
  list(
    c("australian cities", "australian communities",
      "australian experience", "australian society"),
    c(
      "car-based mobility",
      "car-related economic", "car-related economic stress",
      "car-related transport", "car-related transport disadvantage",
      "cost-benefit analysis"
    )
  )

my_search <-
  litsearchr::write_search(
    groupdata = mysearchterms,
    languages = "English",
    stemming = TRUE,
    closure = "none",
    exactphrase = TRUE,
    writesearch = FALSE,
    verbose = TRUE
  )

my_search

gold_standard <-
  c(
    "This article is the second of two papers that review the field of spatially sensitive social scientific research into the links between social status and transport disadvantage.",
    "The first paper undertook a comprehensive review of the social scientific and transport planning literature to mark the level of development in the field and identify conceptual and methodological issues and constraints in this field of inquiry."
  )

title_search <- litsearchr::write_title_search(titles=gold_standard)

retrieved_articles <-
  import_results(file="C:/Users/uqakimpt/OneDrive - The University of Queensland/Desktop/test/My Collection.ris")

retrieved_articles <- litsearchr::remove_duplicates(retrieved_articles, field="title", method="string_osa")

articles_found <- litsearchr::check_recall(true_hits = gold_standard,
                                           retrieved = retrieved_articles$title)

articles_found

https://cran.r-project.org/web/packages/PRISMAstatement/vignettes/PRISMA.html


url_name <- 'https://scholar.google.com/scholar?hl=en&as_sdt=0%2C38&q=transport+disadvantage+australia'
wp <- xml2::read_html(url_name)
# Extract raw data
titles <- rvest::html_text(rvest::html_nodes(wp, '.gs_rt'))
authors_years <- rvest::html_text(rvest::html_nodes(wp, '.gs_a'))
# Process data
authors <- gsub('^(.*?)\\W+-\\W+.*', '\\1', authors_years, perl = TRUE)
years <- gsub('^.*(\\d{4}).*', '\\1', authors_years, perl = TRUE)
# Make data frame
df <- data.frame(titles = titles, authors = authors, years = years, stringsAsFactors = FALSE)


library(websearchr)

google_scholar(my_search)

prisma(found = 36,
       found_other = 0,
       no_dupes = 34, 
       screened = 34, 
       screen_exclusions = 0, 
       full_text = 34,
       full_text_exclusions = 0, 
       qualitative = 34, 
       quantitative = 34,
       width = 800, height = 800)