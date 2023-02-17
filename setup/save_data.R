library(dplyr)
library(readr)
# library(janitor)

# read data from download
# todo(bristow): change to download on run isntead of link
ratings <- read_fwf(
  'C:/Users/brist/Downloads/players_list/players_list_foa.txt',
  col_types = 'icccccccnnnnnnnnnnf',
  skip=1
)

# copy columns
# needed to do this to speed up column type, which required skipping line
# readr::read_fwf doesn't allow to specify first row as colnames
columns = c("ID Number", "Name", "Fed", "Sex", "Tit", "WTit", "OTit", 
            "FOA", "SRtng", "SGm", "SK", "RRtng", "RGm", "Rk", "BRtng", 
            "BGm", "BK", "Byear", "Flag")

# set column names
colnames(ratings) <- columns

# subset to columns of interest
ratings <- ratings[,c(1:6,9,12,15,18)]

# country codes
# todo(bristow) fix some of the `Fed` column to match iso3c

# clean
ratings <- ratings |>
  filter(
    SRtng > 0, # must have rating and rating must be above 0
    Byear > 1900 # must have valid birth year
  ) |>
  mutate(
    Bdecade = factor(sapply(Byear, function(x) x - x %% 10)),
    Age = 2023 - Byear
  )

# save data
saveRDS(ratings, file = 'src/ratings.Rds')

# make list of all distinct countries
# I was struggling to make this in the server side but still pass it 
# to the ui side, so I'm just saving an .Rds object. Also: it's nice 
# to use base R for this sometimes. I always default to tidyverse 
# select |> filter |> summarize but table() works just fine.
ratings$Fed |>
  table() |> 
  names() |> 
  saveRDS(file = 'src/federations.Rds')

# note to self: 30921406 is my FIDE number, but they got my number wrong!