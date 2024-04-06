# connect to MySQL database
library(RMySQL)   

db_user <- 'yiyangjiang'
db_password <- 'Jyy123456'
db_name <- 'SandboxDB'
db_host <- 'cs5200-p2.cmwnfqdoq8fo.us-west-2.rds.amazonaws.com' 
db_port <- 3306 # always this port unless you change it during installation

# 3. Connect to DB
mydb <-  dbConnect(MySQL(), user = db_user, password = db_password,
                   dbname = db_name, host = db_host, port = db_port)

dbExecute(mydb, "
CREATE TABLE journal_fact (
  JournalID INTEGER PRIMARY KEY,
  Title TEXT,
  PubDateYear INTEGER,
  PubQuarter INTEGER,
  PubDateMonth INTEGER,
  NumArticles INTEGER,
  NumUniqueAuthors INTEGER
)")


dbDisconnect(mydb)



library(RSQLite)

# Connect to the SQLite database 
con <- dbConnect(RSQLite::SQLite(), dbname = "publication.db")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS journal_fact (
  JournalID INTEGER,
  Title TEXT,
  PubDateYear INTEGER,
  PubQuarter INTEGER,
  PubDateMonth INTEGER,
  NumArticles INTEGER,
  NumUniqueAuthors INTEGER,
  FOREIGN KEY (JournalID) REFERENCES Journals (JournalID)
)")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS pub_quarters (
  PubQuarter INTEGER PRIMARY KEY,
  QuarterStartMonth INTEGER,
  QuarterEndMonth INTEGER
)")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS pub_months (
  PubMonth INTEGER PRIMARY KEY,
  MonthName TEXT
)")


quarters_data <- data.frame(
  PubQuarter = c(1, 2, 3, 4),
  QuarterStartMonth = c(1, 4, 7, 10),
  QuarterEndMonth = c(3, 6, 9, 12)
)

months_data <- data.frame(
  PubMonth = c(1:12),
  MonthName = c("January", "February", "March", "April", "May", "June",
                "July", "August", "September", "October", "November", "December")
)

dbWriteTable(con, "pub_quarters", quarters_data, append = TRUE, row.names = FALSE)
dbWriteTable(con, "pub_months", months_data, append = TRUE, row.names = FALSE)

library(DBI)
# Define a function to compute the quarter of a given month
get_quarter <- function(month) {
  return(ceiling(month / 3))
}

# Establish a connection to the SQLite database
con <- dbConnect(RSQLite::SQLite(), dbname = "publication.db")

# Query the database to get the required data
data <- dbGetQuery(con, "
SELECT j.JournalID, j.Title, a.ArticleID, j.PubDateYear, j.PubDateMonth, aa.AuthorID
FROM articles AS a
JOIN article_journal AS aj ON a.ArticleID = aj.ArticleID
JOIN journals AS j ON aj.JournalID = j.JournalID
JOIN article_authors AS aa ON a.ArticleID = aa.ArticleID
")

# Convert month abbreviations to month numbers
month_abbreviations <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
data$PubDateMonth <- match(data$PubDateMonth, month_abbreviations)

# Remove rows with missing or invalid values
data <- data[complete.cases(data),]

# Add quarter column to the data
data$PubQuarter <- sapply(data$PubDateMonth, get_quarter)

# Aggregate the data using aggregate() function 
agg_data <- aggregate(cbind(NumArticles = ArticleID, NumUniqueAuthors = AuthorID) ~ JournalID + Title + PubDateYear + PubQuarter + PubDateMonth, data, function(x) length(unique(x)))

# Insert the aggregated data into the fact table
dbWriteTable(con, "journal_fact", agg_data, append = TRUE, row.names = FALSE)

query1 <- "select * from journal_fact limit 10"
result1 <- dbGetQuery(con, query1)
result1


# Read the table from the SQLite database
sqlite_table <- dbReadTable(con, "journal_fact")

# Close the connection when you're done
dbDisconnect(con)



# Import the data into the MySQL table
db_user <- 'yiyangjiang'
db_password <- 'Jyy123456'
db_name <- 'SandboxDB'
db_host <- 'cs5200-p2.cmwnfqdoq8fo.us-west-2.rds.amazonaws.com' 
db_port <- 3306 # always this port unless you change it during installation

# 3. Connect to DB
mydb <-  dbConnect(MySQL(), user = db_user, password = db_password,
                   dbname = db_name, host = db_host, port = db_port)

dbWriteTable(mydb, "journal_fact", sqlite_table, row.names = FALSE, append = TRUE)

query1 <- "select * from journal_fact limit 10"
result1 <- dbGetQuery(mydb, query1)
result1
# Close the MySQL connection
dbDisconnect(mydb)