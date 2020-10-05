# R script for loading and preparing of census data and outlines
# for Brexit analysis.
# Author: Roger Beecham
#####################################

library(rmapshaper)
library(sf)
library(tidyverse)

# Read in shapefile containing GB LA boundaries --  made available from ONS Open Geography Portal.
# We simplify the geometries using the "rmapshaper" library.
download.file("http://geoportal.statistics.gov.uk/datasets/8edafbe3276d4b56aec60991cbddda50_3.zip", "boundaries_gb.zip")
unzip("boundaries_gb.zip")
gb_boundaries <- read_sf("Local_Authority_Districts_December_2015_Super_Generalised_Clipped_Boundaries_in_Great_Britain.shp")
# Set CRS to OSGB.
gb_boundaries <- st_transform(gb_boundaries, crs=27700)
# Simplify polygon. This may take a little time to execute.
gb_boundaries <- ms_simplify(gb_boundaries, keep=0.2)


# Read in Census data that we use to regress on.
census_data <- read_csv("http://homepages.see.leeds.ac.uk/~georjb/ppd/r/data/2011_census_oa.csv")
oa_la_lookup <- read.csv("http://homepages.see.leeds.ac.uk/~georjb/ppd/r/data/oa_la_lookup.csv")
oa_la_lookup$OA <- as.character(oa_la_lookup$OA)
census_data <- left_join(census_data, oa_la_lookup)
# Iterate over OA level data and compute summary statistics on relevant variables to LA level.
census_data <- census_data %>%
  group_by(LOCAL_AUTHORITY_CODE) %>%
  summarise(
    total_pop = sum(Total_Population),
    younger_adults = sum(Age_20_to_24, Age_25_to_29, Age_30_to_44) / sum(Total_Population),
    white = sum(White_British_and_Irish) / sum(Total_Population),
    christian = sum(Christian) / sum(Total_Population),
    english_speaking = sum(Main_language_is_English_or_Main_language_not_English__Can_speak_English_very_well)
    / sum(Total_Population),
    single_ethnicity_household = sum(All_household_members_have_the_same_ethnic_group)
    / sum(Total_Households),
    own_home = sum(Owned_and_Shared_Ownership) / sum(Total_Households),
    not_good_health = sum(Fair_health, Bad_health, Very_bad_health) / sum(Total_Population),
    degree_educated = sum(Highest_level_of_qualification_Level_4_qualifications_and_above) /
      sum(Highest_level_of_qualification_Level_4_qualifications_and_above,
          Highest_level_of_qualification_Level_3_qualifications,
          Highest_level_of_qualification_Level_1_Level_2_or_Apprenticeship,
          No_qualifications),
    no_car = sum(No_cars_or_vans_in_household) / sum(Total_Households),
    private_transport_to_work = sum(Private_Transport) / sum(Total_Employment_16_to_74),
    professionals = sum(Managers_directors_and_senior_officials, Professional_occupations) /
      sum(Total_Employment_16_to_74)
  )

birth_country_11 <- read_csv("http://homepages.see.leeds.ac.uk/~georjb/ppd/r/data/country_of_birth_2011.csv")
birth_country_11 <- left_join(birth_country_11, oa_la_lookup, by=c("oa_code"="OA"))
birth_country_11 <- birth_country_11 %>%
  group_by(LOCAL_AUTHORITY_CODE) %>%
  summarise(total_pop = sum(POPULATION), eu_born = sum(eu_born)) %>%
  transmute(geo_code = LOCAL_AUTHORITY_CODE,
            total_pop = total_pop,
            eu_born = eu_born/total_pop)
attribute_data <- inner_join(census_data, birth_country_11, by=c("LOCAL_AUTHORITY_CODE"="geo_code"))
attribute_data$total_pop.y<- NULL
colnames(attribute_data)[1:2]<- c("geo_code","total_pop")
attribute_data$geo_code <- as.character(attribute_data$geo_code)

# Recode 2011 Census codes to match with codes in referendum data and boundary data
attribute_data$geo_code[attribute_data$geo_code=="E07000097"] <- "E07000242"
attribute_data$geo_code[attribute_data$geo_code=="E07000101"] <- "E07000243"
attribute_data$geo_code[attribute_data$geo_code=="E07000104"] <- "E07000241"
attribute_data$geo_code[attribute_data$geo_code=="E07000100"] <- "E07000240"
attribute_data$geo_code[attribute_data$geo_code=="E08000020"] <- "E08000037"
attribute_data$geo_code[attribute_data$geo_code=="E06000048"] <- "E06000057"

# Join with referendum data.
attribute_data <- inner_join(attribute_data, referendum_data, by=c("geo_code"="Area_Code"))
# Join attribute data with gb boundaries for mapping
data_gb <- gb_boundaries %>% inner_join(attribute_data, by=c("lad15cd"="geo_code"))

# In order keep a clean workspace, remove the redundant data.
rm(census_data)
rm(referendum_data)
rm(attribute_data)
rm(gb_boundaries)
