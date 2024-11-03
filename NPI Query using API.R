##############################################################
## Purpose: Linking health care providers' names to their NPI via
##         interacting with API NPPES 
## Programmer: Haoshu Duan
## Date: 11/3/2024
###############################################################


## Set-up ----
rm(list = ls())

install.packages('httr')
install.packages('jsonlite')
library(httr)
library(jsonlite)
library(purrr)

## 1. Create a function to query the API ----
get_npi <- function(first_name, last_name, postal_code) {
  url = 'https://npiregistry.cms.hhs.gov/api/?version=2.1' ## enter your API link
  
  response <- GET(url, query = list (
    first_name = first_name, 
    last_name = last_name,
    postal_code = postal_code, ## this search for either mailing address or any of practice location addresses match entered addresses
    name_purpose = 'Provider', 
    use_first_name_alias = TRUE, 
    limit = 10)) # the default is 10, but the value can be set to any value from 1 to 200
  
  print(response)

 if(response$status_code == 200) {
    data <-fromJSON(content(response, 'text', encoding = 'UTF-8'))
      if(length(data$results) > 0) {
      ## extract the npi number 
          npi_number <-data$results$number ## extract NPI number 
     
     ## extract the taxonomy group if available 
          taxonomy_group <- data$results$taxonomies[[1]]$desc  ## extract the first taxonomy group
          provider_gender <-data$results$basic$gender
      
      return(list(first_name = first_name, 
                  last_name = last_name,
                  npi_number = npi_number, 
                  taxonomy_group = taxonomy_group, 
                  provider_gender = provider_gender
                 ))
                
    } 
      else {
      return(list(first_name = first_name, 
                  last_name = last_name, 
                  npi_number = NA, 
                  taxonomy_group = NA, 
                  provider_gender = NA))
    }
  } 
  
else {
    stop ('Failed to retrieve data from NPI Registry API')
  }
  
}

get_npi('Margaret', 'Strickland', '22003')
get_npi('Christian', 'Machado', '22003')
get_npi('Rebecca', 'Browne', '22180')

## Apply this npi search function to your dataframe with doctors name and primary zip code in it ----
# write a fake provider name dataframe 
provider_names <- data.frame(
  first_name = c('Margaret',  'Christian', 'Rebecca'),
  last_name = c('Strickland',  'Machado', 'Browne'),
  postal_code = c('22003', '22003', '22180')
)

# Get NPI and taxonomy 
results <-pmap_dfr(list(provider_names$first_name, 
                        provider_names$last_name, 
                        provider_names$postal_code),
                   get_npi)



