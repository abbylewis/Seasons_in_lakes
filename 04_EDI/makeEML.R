##MakeEML - Seasons
##Author: Abby Lewis
setwd("04_EDI")

#good site for step-by-step instructions
#https://ediorg.github.io/EMLassemblyline/articles/overview.html
#and links therein

# (install and) Load EMLassemblyline #####
# install.packages('devtools')

#devtools::install_github("EDIorg/EMLassemblyline")
library(EMLassemblyline)
library(tidyverse)

#Step 1: Create a directory for your dataset

#Step 2: Move your dataset to the directory
  
#Step 3: Identify an intellectual rights license
#ours is CCBY

#Step 4: Identify the types of data in your dataset

#Step 5: Import the core metadata templates

# View documentation for these functions
?template_core_metadata
?template_table_attributes
?template_categorical_variables #don't run this till later
?template_geographic_coverage

# Import templates for our dataset licensed under CCBY, with 1 table.
template_core_metadata(path = ".",
                 license = "CCBY",
                 file.type = ".txt",
                 write.file = TRUE)

template_table_attributes(path = ".",
                       data.path = ".",
                       data.table = c("Met.csv", "Lake_data.csv"),
                       write.file = TRUE)

template_geographic_coverage(path = ".",
                          data.path = ".",
                          empty = T,
                          write.file = TRUE)

#Step 6: Script your workflow
#that's what this is, silly!

#Step 7: Abstract
#copy-paste the abstract from your Microsoft Word document into abstract.txt
#if you want to check your abstract for non-allowed characters, go to:
#https://pteo.paranoiaworks.mobi/diacriticsremover/
#paste text and click remove diacritics

#Step 8: Methods
#copy-paste the methods from your Microsoft Word document into abstract.txt
#if you want to check your abstract for non-allowed characters, go to:
#https://pteo.paranoiaworks.mobi/diacriticsremover/
#paste text and click remove diacritics

#Step 9: Additional information

#Step 10: Keywords

#Step 11: Personnel

#Step 12: Attributes
#for units....
# View and search the standard units dictionary
#view_unit_dictionary()

#Step 13: Close files
#if all your files aren't closed, sometimes functions don't work

#Step 14: Categorical variables
# Run this function for your dataset
template_categorical_variables(path = ".",
                               data.path = ".",
                               write.file = TRUE)

#Step 15: Geographic coverage

## Step 16: Obtain a package.id. ####
# Go to the EDI staging environment (https://portal-s.edirepository.org/nis/home.jsp),
# then login

# Select Tools --> Data Package Identifier Reservations and click 
# "Reserve Next Available Identifier"
# A new value will appear in the "Current data package identifier reservations" 
# table (e.g., edi.123)
# Make note of this value, as it will be your package.id below

#Step 17: Make EML
# View documentation for this function
?make_eml

# Run this function
make_eml(
  path = ".",
  data.path = ".",
  eml.path = ".",
  dataset.title = "Seasonality of in-lake and meteorological data from Midway Pond (USA) and Lake Rerewhakaaitu (NZ), including daily measurements of water temperature, chlorophyll-a, dissolved oxygen, air temperature, and solar radiation",
  temporal.coverage = c("2021-02-03", "2024-06-11"),
  maintenance.description = 'completed',
  data.table = c("Lake_data.csv",
                 "Met.csv"),
  data.table.name = c("In-lake data",
                      "Meteorological data"),
  data.table.description = c("In-lake sensor data from both lakes",
                             "Meterological data from both lakes"),
  user.id = 'alewis',
  user.domain = 'EDI',
  package.id = 'edi.1312.2')

## Step 8: Check your data product! ####
# Return to the EDI staging environment (https://portal-s.edirepository.org/nis/home.jsp),
# then login

# Select Tools --> Evaluate/Upload Data Packages, then under "EML Metadata File", 
# choose your metadata (.xml) file (e.g., edi.270.1.xml), check "I want to 
# manually upload the data by selecting files on my local system", then click Upload.

# Now, Choose File for each file within the data package (e.g., each zip folder), 
# then click Upload. Files will upload and your EML metadata will be checked 
# for errors. If there are no errors, your data product is now published! 
# If there were errors, click the link to see what they were, then fix errors 
# in the xml file. 
# Note that each revision results in the xml file increasing one value 
# (e.g., edi.270.1, edi.270.2, etc). Re-upload your fixed files to complete the 
# evaluation check again, until you receive a message with no errors.

## Step 9: PUBLISH YOUR DATA! ####
# Go to the EDI Production environment (https://portal.edirepository.org/nis/home.jsp)
# and login 

# Select Tools --> Data Package Identifier Reservations and click "Reserve Next 
# Available Identifier". A new value will appear in the "Current data package 
# identifier reservations" table (e.g., edi.518)
# This will be your PUBLISHED package.id

# In the make_eml command below, change the package.id to match your 
# PUBLISHED package id. This id should end in .1 (e.g., edi.518.1)

# ALL OTHER entries in the make_eml() command should match what you ran above,
# in step 7

make_eml(
  path = ".",
  data.path = ".",
  eml.path = ".",
  dataset.title = "Seasonality of in-lake and meteorological data from Midway Pond (USA) and Lake Rerewhakaaitu (NZ), including daily measurements of water temperature, chlorophyll-a, dissolved oxygen, air temperature, and solar radiation",
  temporal.coverage = c("2021-02-03", "2024-06-11"),
  maintenance.description = 'completed',
  data.table = c("Lake_data.csv",
                 "Met.csv"),
  data.table.name = c("In-lake data",
                      "Meteorological data"),
  data.table.description = c("In-lake sensor data from both lakes",
                             "Meterological data from both lakes"),
  user.id = 'alewis',
  user.domain = 'EDI',
  package.id = 'edi.1312.2')

# Once your xml file with your PUBLISHED package.id is Done, return to the 
# EDI Production environment (https://portal.edirepository.org/nis/home.jsp)

# Select Tools --> Preview Your Metadata, then upload your metadata (.xml) file 
# associated with your PUBLISHED package.id. Look through the rendered 
# metadata one more time to check for mistakes (author order, bounding box, etc.)

# Select Tools --> Evaluate/Upload Data Packages, then under "EML Metadata File", 
# choose your metadata (.xml) file associated with your PUBLISHED package.id 
# (e.g., edi.518.1.xml), check "I want to manually upload the data by selecting 
# files on my local system", then click Upload.

# Now, Choose File for each file within the data package (e.g., each zip folder), 
# then click Upload. Files will upload and your EML metadata will be checked for 
# errors. Since you checked for and fixed errors in the staging environment, this 
# should run without errors, and your data product is now published! 

# Click the package.id hyperlink to view your final product! HOORAY!