library(RODBC)
library(odbc)

#ActaTool
source("C:/GitHub/AdantiaTools/00_functions.R")

BD <- "C:/GitHub/AdantiaTools/01_Recursos/04_BDD"

conAccdb <- odbcDriverConnect(BD) 

library(odbc)
library(DBI)

cs <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=C:/GitHub/AdantiaTools/01_Recursos/04_BDD.mdb"
con <- dbConnect(odbc::odbc(), .connection_string = cs)

conn <- odbcConnectAccess2007(path.expand("C:/GitHub/AdantiaTools/01_Recursos/04_BDD.mdb")) 
subset(sqlTables(conn), TABLE_TYPE == "TABLE") 
df <- sqlFetch(conn, "Table1") 
close(conn) 

conn <- odbcConnect("MS Access Database;DBQ=C:/GitHub/AdantiaTools/01_Recursos/04_BDD.mdb")
df <- sqlQuery(conn, "SELECT * FROM myTable")
odbcClose(conn)