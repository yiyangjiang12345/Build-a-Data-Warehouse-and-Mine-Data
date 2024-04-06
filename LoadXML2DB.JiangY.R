library(RSQLite)

# Connect to the SQLite database 
con <- dbConnect(RSQLite::SQLite(), dbname = "publication.db")

# Create the Authors table
dbExecute(con, "
CREATE TABLE IF NOT EXISTS authors (
  AuthorID INTEGER PRIMARY KEY,
  LastName TEXT,
  ForeName TEXT,
  Initials TEXT
)")

# Create the Journals table
dbExecute(con, "
CREATE TABLE IF NOT EXISTS journals (
  JournalID INTEGER PRIMARY KEY,
  ISSN TEXT,
  IssnType TEXT,
  Volume TEXT,
  Issue TEXT,
  Title TEXT,
  PubDateYear INTEGER,
  PubDateMonth TEXT
)")

# Create the Articles table
dbExecute(con, "
CREATE TABLE IF NOT EXISTS articles (
  ArticleID INTEGER PRIMARY KEY,
  PMID TEXT NOT NULL UNIQUE,
  ArticleTitle TEXT
)")

# Create the Article_Authors table
dbExecute(con, "
CREATE TABLE IF NOT EXISTS article_authors (
  ArticleID INTEGER,
  AuthorID INTEGER,
  FOREIGN KEY (ArticleID) REFERENCES Articles (ArticleID),
  FOREIGN KEY (AuthorID) REFERENCES Authors (AuthorID)
)")

# Create the Article_Journal table
dbExecute(con, "
CREATE TABLE IF NOT EXISTS article_journal (
  ArticleID INTEGER,
  JournalID INTEGER,
  FOREIGN KEY(ArticleID) REFERENCES Articles(ArticleID),
  FOREIGN KEY (JournalID) REFERENCES Journals (JournalID)
)")

# Close the database connection
dbDisconnect(con)

#read xml file
library(XML)
xmlFile <- "pubmed-tfm-xml/pubmed22n0001-tf.xml"


dom <- xmlParse(xmlFile, validate=T)

# Extract list of Article nodes
articles <- getNodeSet(dom, "//Article")
article_data <- data.frame(ArticleID = integer(), PMID = character(), ArticleTitle = character(), stringsAsFactors = FALSE)
author_data <- data.frame(AuthorID = integer(), LastName = character(), ForeName = character(), Initials = character(), stringsAsFactors = FALSE)
journal_data <- data.frame(JournalID = integer(), ISSN = character(), IssnType = character(), Volume = character(), Issue = character(), Title = character(), PubDateYear = integer(), PubDateMonth = character(), stringsAsFactors = FALSE)
article_author_data <- data.frame(ArticleID = character(), AuthorID = integer(), stringsAsFactors = FALSE)
article_journal_data <- data.frame(ArticleID = character(), JournalID = integer(), stringsAsFactors = FALSE)

author_id <- 1
journal_id <- 1
author_map <- list()
journal_map <- list()

# Loop through all the articles
for (article in articles) {
  # Assign a unique ArticleID
  ArticleID <- max(article_data$ArticleID, default = 0) + 1
  # Extract PMID (PubMed ID) attribute from the article XML
  PMID <- xmlAttrs(article)["PMID"]
  # Extract the ArticleTitle from the article XML
  ArticleTitle <- xmlValue(getNodeSet(article, ".//ArticleTitle")[[1]])
  # Add the new article data to the article_data data frame
  article_data <- rbind(article_data, data.frame(ArticleID = ArticleID, PMID = PMID, ArticleTitle = ArticleTitle))
  # Reset row names in article_data
  row.names(article_data) <- NULL
  
  # Check if the article has an AuthorList
  if (length(getNodeSet(article, ".//AuthorList")) > 0) {
    # Get all the authors
    authors <- getNodeSet(article, ".//Author")
    # Loop through all the authors
    for (author in authors) {
      # Extract LastName, ForeName, and Initials from the author XML, or assign NA if not present
      LastName <- ifelse(length(getNodeSet(author, ".//LastName")) > 0, xmlValue(getNodeSet(author, ".//LastName")[[1]]), NA)
      ForeName <- ifelse(length(getNodeSet(author, ".//ForeName")) > 0, xmlValue(getNodeSet(author, ".//ForeName")[[1]]), NA)
      Initials <- ifelse(length(getNodeSet(author, ".//Initials")) > 0, xmlValue(getNodeSet(author, ".//Initials")[[1]]), NA)
      # Create a unique key for the author
      author_key <- paste(LastName, ForeName, Initials, sep = "|")
      
      # If the author is not already in the author_map, add them to the author_data data frame and update the author_map
      if (!author_key %in% names(author_map)) {
        author_data <- rbind(author_data, data.frame(AuthorID = author_id, LastName = LastName, ForeName = ForeName, Initials = Initials))
        author_map[[author_key]] <- author_id
        author_id <- author_id + 1
      }
      # Add the relationship between the article and author to the article_author_data data frame
      article_author_data <- rbind(article_author_data, data.frame(ArticleID = PMID, AuthorID = author_map[[author_key]]))
      row.names(article_author_data) <- NULL
    }
  }
  
  # Check if the article has a Journal
  if (length(getNodeSet(article, ".//Journal")) > 0) {
    # Get the journal XML node
    journal <- getNodeSet(article, ".//Journal")[[1]]
    # Extract ISSN, IssnType, Volume, Issue, Title, PubDateYear, and PubDateMonth from the journal XML, or assign NA if not present
    ISSN <- ifelse(length(getNodeSet(journal, ".//ISSN")) > 0, xmlValue(getNodeSet(journal, ".//ISSN")[[1]]), NA)
    IssnType <- ifelse(length(getNodeSet(journal, ".//ISSN")) > 0, xmlGetAttr(getNodeSet(journal, ".//ISSN")[[1]], "IssnType"), NA)
    Volume <- ifelse(length(getNodeSet(journal, ".//Volume")) > 0, xmlValue(getNodeSet(journal, ".//Volume")[[1]]), NA)
    Issue <- ifelse(length(getNodeSet(journal, ".//Issue")) > 0, xmlValue(getNodeSet(journal, ".//Issue")[[1]]), NA)
    Title <- xmlValue(getNodeSet(journal, ".//Title")[[1]])
    PubDateYear <- ifelse(length(getNodeSet(journal, ".//PubDate/Year")) > 0, as.integer(xmlValue(getNodeSet(journal, ".//PubDate/Year")[[1]])), NA)
    PubDateMonth <- ifelse(length(getNodeSet(journal, ".//PubDate/Month")) > 0, xmlValue(getNodeSet(journal, ".//PubDate/Month")[[1]]), NA)
    # Create a unique key for the journal
    journal_key <- paste(ISSN, Title, sep = "|")
    
    # If the journal is not already in the journal_map, add it to the journal_data data frame and update the journal_map
    if (!journal_key %in% names(journal_map)) {
      journal_data <- rbind(journal_data, data.frame(JournalID = journal_id, ISSN = ISSN, IssnType = IssnType, Volume = Volume, Issue = Issue, Title = Title, PubDateYear = PubDateYear, PubDateMonth = PubDateMonth), row.names = NULL)
      journal_map[[journal_key]] <- journal_id
      journal_id <- journal_id + 1
    }
    # Add the relationship between the article and journal to the article_journal_data data frame
    article_journal_data <- rbind(article_journal_data, data.frame(ArticleID = PMID, JournalID = journal_map[[journal_key]]), row.names = NULL)
  }
}

# Connect to the SQLite database
db <- dbConnect(RSQLite::SQLite(), dbname = "publication.db")

# Save data frames to the appropriate tables
dbWriteTable(db, "authors", author_data, row.names = FALSE, overwrite = TRUE)
dbWriteTable(db, "journals", journal_data, row.names = FALSE, overwrite = TRUE)
dbWriteTable(db, "articles", article_data, row.names = FALSE, overwrite = TRUE)
dbWriteTable(db, "article_authors", article_author_data, row.names = FALSE, overwrite = TRUE)
dbWriteTable(db, "article_journal", article_journal_data, row.names = FALSE, overwrite = TRUE)

#check the table is work
query1 <- "select * from journals limit 10"
result1 <- dbGetQuery(db, query1)
result1

# Close the database connection
dbDisconnect(db)