######## Short SQLdf tutorial to integrate SQL into R #########
# https://www.r-bloggers.com/accessing-mysql-through-r/

# Connecting to MySQL is made very easy with the RMySQL package. To connect to a MySQL database simply install the package and load the library.

# https://www.slideshare.net/RsquaredIn/rmysql-tutorial-for-beginners

install.packages("RMySQL")
library(RMySQL)

# Connecting to MySQL:
# Once the RMySQL library is installed create a database connection object.

mydb = dbConnect(MySQL(), user='cmc454', password='Arlington15*', dbname='database_name', host='host')

#Listing Tables and Fields:
# Now that a connection has been made we list the tables and fields in the database we connected to.

dbListTables(mydb)

# This will return a list of the tables in our connection. 

dbListFields(mydb, 'some_table')

# This will return a list of the fields in some_table.

dbDataType(RMySQL::MySQL(), "a")

# We need to specify the driver details as well as the object to test the SQL data type

# Running Queries:
# Queries can be run using the dbSendQuery function.

dbSendQuery(mydb, 'drop table if exists some_table, some_other_table')

# SEE BELOW FOR HOW TO DO THIS AND SAVE IT INTO R
  # Also, if I wanted to import a table in its entirety, I could just use dbReadTable()
dbReadTable(mydb, "some_table")
  # I could sned query and retreive results in one swoop with dbGetQuery()
dbGetQuery(mydb, "SELECT * FROM some_table LIMIT 5")
# We just write the MySQL language between the parantheses so so long as those are in the front and back we can structure it just like we normally would in MySQL with indentations and everything. Like in Python, it send it as a string to the server. 

# In my experience with this package any SQL query that will run on MySQL will run using this method

# Making tables:
# We can create tables in the database using R dataframes.

dbWriteTable(mydb, name='table_name', value=data.frame.name)

# Probably won't ever be going this direction, but good to know

# Retrieving data from MySQL:
#To retrieve data from the database we need to save a results set object.

rs = dbSendQuery(mydb, "select * from some_table")

# I believe that the results of this query remain on the MySQL server, to access the results in R we need to use the fetch function.

data = fetch(rs, n=-1)

# This saves the results of the query as a data frame object. The n in the function specifies the number of records to retrieve, using n=-1 retrieves all pending records.

# Advantage of doing it this way over dbReadTable() or dbGetQuery() is that it imports data in batches - Can increase the n to do more batches. 


# TO DISCONNECT FROM THE DATABASE
dbDisconnect(mydb)
