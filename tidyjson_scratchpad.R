library(tidyverse)
library(tidyjson)
library(jsonlite)
library(httr)

# https://cran.r-project.org/web/packages/tidyjson/vignettes/introduction-to-tidyjson.html


#//////////////////////////////////////


# tidyjson is an easier interfact for manipulating json using tidy tools like dplyr (see comparison below)

# get json with httr and parse
response <- GET("https://api.exchangerate.host/timeseries?start_date=2020-01-01&end_date=2020-01-04&base=UAH&symbols=USD")
http_type(response)
content(response, as = "text")
content(response, as = "parsed") 

# get json with jsonlite (like parsed response from httr)
fromJSON("https://api.exchangerate.host/timeseries?start_date=2020-01-01&end_date=2020-01-04&base=UAH&symbols=USD") 

# get json with tidyjson
content(GET("https://api.exchangerate.host/timeseries?start_date=2020-01-01&end_date=2020-01-04&base=UAH&symbols=USD"), 
        as = "text") %>% spread_all() %>% glimpse()
content(GET("https://api.exchangerate.host/timeseries?start_date=2020-01-01&end_date=2020-01-04&base=UAH&symbols=USD"), 
        as = "text") %>% spread_all() %>% select(starts_with("rates"))
content(GET("https://api.exchangerate.host/timeseries?start_date=2020-01-01&end_date=2020-01-04&base=UAH&symbols=USD"), 
        as = "text") %>% spread_all() %>% select(starts_with("rates")) %>% as_tibble()
content(GET("https://api.exchangerate.host/timeseries?start_date=2020-01-01&end_date=2020-01-04&base=UAH&symbols=USD"), 
        as = "text") %>% spread_all() %>% select(starts_with("rates")) %>% as_tibble() %>%
        pivot_longer(cols = everything(), names_to = "var", values_to = "values")


#/////////////////////////////


# create get_historic_exchange_rates()
get_historic_exchange_rates <- function(date_from, date_to, from_currency, to_currency, ...) {
        
        # create api call
        api_call <- str_c("https://api.exchangerate.host/timeseries?start_date=", date_from, "&end_date=",
                          date_to, "&base=", from_currency, "&symbols=", to_currency)
        print(api_call)
        
        content(GET(api_call), as = "text") %>% spread_all() %>% select(base, starts_with("rates")) %>% as_tibble() %>%
                pivot_longer(cols = -base, names_to = "var", values_to = "exchange_rate") %>%
                separate(col = var, into = c("extra", "date_currency"), sep = "\\.", extra = "merge", remove = FALSE) %>%
                separate(col = date_currency, into = c("date", "to_currency"), sep = "\\.", extra = "merge", remove = FALSE) %>%
                mutate(date = ymd(date), corrupt_multi_record_response_flag = case_when(nrow(.) > 1 ~ 1, TRUE ~ 0)) %>% 
                rename(from_currency = base) %>%
                select(date, from_currency, to_currency, exchange_rate, corrupt_multi_record_response_flag) %>% return()
}        

# get historic_exchange_rates
historic_exchange_rates <- metric_2b %>% filter(currency != "USD") %>% select(date_reported, currency, exchange_rate) %>%
        mutate(date_from = date_reported, date_to = date_reported, from_currency = "USD", to_currency = currency) %>%
        pmap(.l = ., .f = ~ get_historic_exchange_rates(date_from, date_to, from_currency, to_currency, ...)) %>%
        bind_rows()

historic_exchange_rates
historic_exchange_rates %>% glimpse()
# map(.x = historic_exchange_rates, .f = ~ nrow(.x))
historic_exchange_rates %>% count(to_currency)
historic_exchange_rates %>% count(date)
metric_2b %>% glimpse()


#//////////////////////////////////////


api_call <- "https://www.transparency.treasury.gov/services/api/fiscal_service/v1/accounting/od/exchange_rates?fields=country_currency_desc,exchange_rate,data_date&filter=country_currency_desc:in:(GEORGIA-LARI),data_date:gte:2018-01-01"
content(GET(api_call), as = "text") %>% spread_all() %>% glimpse()
content(GET(api_call), as = "text") %>% gather_object() %>% json_types()
content(GET(api_call), as = "text") %>% enter_object(data) %>% gather_array() %>% spread_all()


#//////////////////////////////////////


# Define a simple people JSON collection
people <- c('{"age": 32, "name": {"first": "Bob",   "last": "Smith"}}',
            '{"age": 54, "name": {"first": "Susan", "last": "Doe"}}',
            '{"age": 18, "name": {"first": "Ann",   "last": "Jones"}}')
people

# with jsonlite
fromJSON(people)


# Tidy the JSON data
people %>% spread_all()
people %>% spread_all() %>% glimpse()
people %>% spread_all() %>% as_tibble()


#///////////////////////////////////////


worldbank %>% str
worldbank %>% spread_all()
worldbank %>% spread_all() %>% glimpse()

# but note that if json contains arrays, these cannot be naively spread, and so spread omits them
# worldbank has an array for majorsector_percent
worldbank %>% gather_object()
worldbank %>% gather_object() %>% json_types()
worldbank %>% gather_object() %>% json_types() %>% count(name, type)

worldbank %>% enter_object(majorsector_percent)
worldbank %>% enter_object(majorsector_percent) %>% gather_array()
worldbank %>% enter_object(majorsector_percent) %>% gather_array() %>% spread_all()

# can combine normal spread_all operations with array operations using enter_object
# can do normal dplyr select, rename, group_by, summarize, etc
worldbank %>%
        spread_all() %>% select(regionname, totalamt) %>%
        enter_object(majorsector_percent) %>% gather_array() %>% 
        spread_all() %>% rename(sector = Name, percent = Percent) %>%
        group_by(regionname, sector) %>%
        summarize(funding = sum(totalamt * percent))




