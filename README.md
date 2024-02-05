# fewapo

This repository contains R language scripts for scraping and cleaning the data on persons killed by police from public sources, merging the data for WA state, and producing a couple of reports.

We scrape data from three sources:

* the [Fatal Encounters Project](https://fatalencounters.org/) (data from 2000-2021)
* the [Washington Post police shootings database](https://www.washingtonpost.com/graphics/investigations/police-shootings-database) (data from 2015 forward) 
* the [Mapping Police Violence project](https://mappingpoliceviolence.us) (data from 2013 forward)

There is substantial overlap in coverage across these sources, but the Fatal Encounters (FE) dataset goes back farther and includes people killed by other weapons, and by police pursuits.  FE stopped updating in 2021, but Mapping Police Violence (MPV) has largely taken over the FE project data, updating with people shot by police, and adding many additional variables on legal outcomes, community characteristics and US legislative jurisdictions.

This project is mostly focused on WA State.  So, for WA only, 

* We update the FE data post 2021 with their original inclusion criteria and variables.
* We merge the data from FE and the Washington Post (WaPo)
* At this time we are not yet merging the data from MPV, but that is the eventual goal

For WA only, we maintain [an online report](https://rpubs.com/moxbox/wa_since2015) updated each week tracking the trends in people killed by police in WA since 2015.  The script for that report is in the `Analysis` folder of this repository.

For the US, we have another [online report](https://rpubs.com/moxbox/statepercapitatrends) that uses the FE data from 2000-2021 to track the per capita trends in people killed by police over time by state.  

We are committed to accurate, transparent, reproducible research.  All of the tools used in our workflow are open-source and free to the public.  A few notes follow on the the contents of each folder.

____

## The `Data` folder

Has subfolders for raw and cleaned data.  

* `Raw` -- Most of the data used by the scripts is scraped online, so there are only 2 files here:  
  * WA updates to the FE dataset, in an excel file  
  * GIS shapefiles for the US per capita report, in a json file (source url provided in `USpercapitaTrends.Rmd` script)

* `Clean` -- just one file with 2024 Legislative District information for WA State.

## The `Construction` folder 

This contains the key files needed to scrape, clean and construct the WA merged dataset, and the population data by state for the US:

* `MakeData` - main script for data construction, calls the other scripts:
  * `scrapeDataSets` - scrapes and reads in all needed data
  * `fixes_preclean_XX` - fixes errors identified in each dataset. These are typically found during the merge process.
  * `clean_XX` - harmonized variable construction for each dataset.
  *  `postcleaning` - corrections applied before the WA state FE-WaPo merge.  These corrections are meant to be temporary, to address lags in updating that prevent successful merges, and they are deleted or inactivated when no longer needed.

* `StateCensusData` - script for scraping and managing census population estimates, used to calculate *per capita* rates for the US report.


## The `Analyses` folder

This contains the scripts that use the cleaned/merged data to produce reports that are hosted on rpubs.com:

* [WA since 2015](https://rpubs.com/moxbox/wa_since2015) -- updated weekly.  To run this report you need to run the `MakeData` script first.

* [US per capita trends by state 2000-2021](https://rpubs.com/moxbox/statepercapitatrends) -- uses only FE data given the time frame.  To run this report, you need to source the `StateCensusData` script first.

### `Common` subfolder

The `Common` subfolder includes a set of scripts/rmarkdown files that are used by the WA since 2015 report.  They are called by the rmarkdown file via either `source` or the chunk option `child=`.

### Note the yaml params

For the WA since 2015 report, these provide an option for modifying the starting date.  To facilitate maintenance, these small differences are captured in the yaml as `params`, and the `params` are referred to in the report markdown. 

____

*Find a bug/error?*  Please report it as an issue, thx!

*Want to get involved?* Please contact the repository maintainer
