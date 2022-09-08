library(RPostgres)
library(DBI)

db <- 'defaultdb'
host_db <- 'free-tier14.aws-us-east-1.cockroachlabs.cloud' #i.e. # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
db_port <- '26257'  # or any other port specified by the DBA
db_user <- 'analytics'
db_password <- 'otJogSdHKvzhpkXLPdwV0w'

con <- dbConnect(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user,
                 password=db_password, sslmode = "verify-full", options = "--cluster=bullpen-notes-4213")
