library("lubridate")
library(httr)

#link <- "https://www.ecb.europa.eu/paym/coll/assets/html/dla/ea_MID/ea_csv_210108.csv"

# Concatenates the date with the url, to get the url-string
get_url_string <- function(date){
  return(paste0("https://www.ecb.europa.eu/paym/coll/assets/html/dla/ea_MID/ea_csv_", format(date, "%y%m%d"),".csv"))
}

# Find last date where data was published (on weekend and holidays nothing is published). 
# If no data published today go that many days back
max_days_back <- 10

#Start with next day, since this data is published on 6:30 ECT on the previous day
retrieval_date <- today()+1

# Iterate over days and go back until max_day_back reached or success in url
while(http_status(GET(get_url_string(retrieval_date)))$category != "Success" || as.numeric(today()+1-retrieval_date) > max_days_back){
  retrieval_date <- retrieval_date-1
}

# If no url found, even though we went max_day_back then throw error
if(as.numeric(today()+1-retrieval_date) > max_days_back){
  stop("File not found on ECB-Server")
}

# If url found, then read in dataframe
fiona <- as_tibble(read.delim(paste0("https://www.ecb.europa.eu/paym/coll/assets/html/dla/ea_MID/ea_csv_", format(retrieval_date, "%y%m%d"),".csv"), fileEncoding="UTF-16"))
