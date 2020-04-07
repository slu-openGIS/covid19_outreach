# Download all of Google's COVID Mobility Reports and Parse the Data
# See this Page : https://www.google.com/covid19/mobility/

# Set a Folder Here and Source the Script
folder='~/Desktop/Covid_Mobility/'

# Standardize State Names
states <- gsub('\\s', '_', state.name)

# Download all of the Reports
for(state in states){
  url <- paste0('https://www.gstatic.com/covid19/mobility/2020-03-29_US_', state, '_Mobility_Report_en.pdf')
  destfile <- paste0(folder, state, '_Mobility_Report_en.pdf')
  download.file(url, destfile)
}

# Parse the Text from the PDF
library(pdftools)
library(stringr)

# Parse Page
parse_pg <- function(pg){
  a=str_remove_all(pg, '\\s')
  a=unlist(str_split(a, 'SunFeb16SunMar8SunMar29SunFeb16SunMar8SunMar29SunFeb16SunMar8SunMar29'))
  a=unlist(str_split(a, 'Retail&recreation\\**Grocery&pharmacy\\**Parks\\**'))
  a=unlist(str_split(a, 'Transitstations\\**Workplace\\**Residential\\**'))
  a=str_remove(a, '\\+80\\%\\+80\\%\\+80\\%\\+40\\%\\+40\\%\\+40\\%BaselineBaselineBaseline-40\\%-40\\%-40\\%-80\\%-80\\%-80\\%')
  a=unlist(str_split(a, 'Notenoughdataforthisdate'))
  a=unlist(str_split(a, 'comparedtobaseline'))
  
  if(a[11] == ''){ # Single County
    df = data.frame(stringsAsFactors = FALSE,
      County = a[1],
      Retail_Recreation = a[2],
      Grocery_Pharma = a[3],
      Parks = a[4],
      Transit = a[7],
      Workplace = a[8],
      Residential = a[9]
    )
  }else{ # Two Counties
    df = data.frame(stringsAsFactors = FALSE,
      County = c(a[1], a[11]),
      Retail_Recreation = c(a[2], a[12]),
      Grocery_Pharma = c(a[3], a[13]),
      Parks = c(a[4], a[14]),
      Transit = c(a[7], a[17]),
      Workplace = c(a[8], a[18]),
      Residential = c(a[9], a[19])
    )
  }
  
  return(df)
}

# Parse Document
parse_pdf <- function(file){
  txt <- pdf_text(file)
  pages <- txt[3:(length(txt)-1)]
  res <- vector('list', length(pages))
  for ( i in seq_along(pages)){
    res[[i]] <- parse_pg(pages[i])
  }
  
  res <- do.call(rbind, res)
  return(res)
}

# Parse All of the States in The Downloaded Directory
# Adding State as a Variable to the Data.frames
library(dplyr)
library(magrittr)

all_data <- vector('list', length(states))
for( i in seq_along(states) ){
  file <- paste0(folder, states[i], '_Mobility_Report_en.pdf')
  all_data[[i]] <- parse_pdf(file)
  all_data[[i]] <- mutate(all_data[[i]], State = states[i])
}

us_data <- bind_rows(all_data)

# Clean the Final Dataframe a bit for Mapping
to_num <- function(var){
  str_remove(var, '\\%') %>%
    as.numeric() %>%
    `/`(., 100)
}

us_data %<>% 
  mutate(
    County = gsub("([a-z])([A-Z])", "\\1 \\2", County),
    State = str_replace(State, '_', ' '),
    Retail_Recreation = to_num(Retail_Recreation),
    Grocery_Pharma = to_num(Grocery_Pharma),
    Parks = to_num(Parks),
    Transit = to_num(Transit),
    Workplace = to_num(Workplace),
    Residential = to_num(Residential)
  )

save(us_data, file = 'data/google_mobility.rda')
