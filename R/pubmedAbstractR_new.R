


pubmedAbstractR_new <- function (search, n = 1000, ncbi_key = NA, start = 2000, end = end, 
          db = "pubmed", keyword = FALSE, authors = FALSE, citations = FALSE) 
{
  require(RISmed)
  require(stringr)
  require(dplyr)
  require(purrr)
  require(tibble)
  require(glue)
  search <- sprintf("%s&api_key=%s", gsub(" ", "+", search), 
                    ncbi_key)
  n <- n
  start <- start
  end <- end
  s1 <- EUtilsSummary(search, type = "esearch", db = "pubmed", 
                      datetype = "pdat", retmax = n, mindate = start, maxdate = end)
  comment <- glue::glue("Please wait...Your query is ", {
    s1@querytranslation
  }, ". This returns ", {
    s1@count
  }, " abstracts. ", "By default 1000 abstracts are downloaded. You downloaded ", 
  n, " abstracts. To retrieve more set 'n =' argument to the desired value")
  print(comment)
  fetch <- EUtilsGet(s1, type = "efetch", db = "pubmed")
  DOI = fetch@PMID
  abstracts <- as_tibble(cbind(title = fetch@ArticleTitle, 
                               abstract = fetch@AbstractText, journal = fetch@ISOAbbreviation, 
                               pmid = fetch@PMID, year = fetch@YearPubmed))
  fetch@Citations %>%
    enframe()
                               
  if (keyword == TRUE) {
    mesh <- purrr::map(fetch@Mesh, "Heading") %>%
      enframe() %>%
      rename(pmid = name, keyword = value) %>%
      unnest("keyword")
      
    #names(mesh) <- pmid
    # mesh <- mesh %>% bind_rows(., .id = "pmid") %>% rename(keyword = value) %>% 
    #   data.frame()
    abstracts <- left_join(abstracts, mesh, by = "pmid") %>% 
      group_by(title, abstract, journal, year, pmid) %>% 
      summarise(keyword = paste(keyword, collapse = ", "))
  }
  if (authors == TRUE) {
    library(magrittr)
    authors <- purrr::map(fetch@Author, extract, c("LastName", 
                                                   "Initials", "order") ) %>% 
                            purrr::map(., data.frame)
    pmid <- fetch@PMID
    names(authors) <- pmid
    authors <- authors %>% bind_rows(., .id = "pmid") %>% 
      data.frame()
    abstracts <- left_join(abstracts, authors)
  }
  if (citations == TRUE) {
  citations <- fetch@Citations %>%
    enframe() %>%
    unnest("value") %>%
    rename(
      pmid = name, 
      refs = value
    ) %>%
    unnest("refs") %>%
    DT::datatable(filter = "top", 
                  options = list(pageLength = 50))
    
  }
  out <- list(abstracts = abstracts, n_articles = s1@count, 
              search = s1@querytranslation, 
              references = citations)
}


