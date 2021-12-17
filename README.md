# fewapo

This repository contains R language scripts for scraping, cleaning and merging the data on persons killed by police from the [Fatal Encounters Project](https://fatalencounters.org/) and the [Washington Post police shootings database](https://www.washingtonpost.com/graphics/investigations/police-shootings-database).

We are committed to accurate, transparent, reproducible research.  All of the tools used in our workflow are open-source and free to the public.

____

*If you use these data or scripts, please consider [donating to Fatal Encounters](https://fatalencounters.org/donate/)*

____

## The `DataCleaningScripts` folder 

This contains the key files needed to construct the merged dataset and population counts:

* `ScrapeMerge` - the main scraping / cleaning / merging / datafile construction script
* `fixes` - called by `ScrapeMerge` to spot fix errors identified in either dataset, these are applied to the final merged dataset.
* `StateCensusData` - script for scraping and managing census population estimates, used to calculate *per capita* rates for the US report

Running/sourcing the `ScrapeMerge` script will create the cleaned datafiles needed for the WA state reports below.

If you want to run the US national per capita analysis, you will also need to source the `StateCensusData` script

## The `Analyses` folder

This contains the scripts that used the cleaned data to produce reports that are hosted on rpubs:

*Washington state*  
* [WA since the passage of Inititative-940](https://rpubs.com/moxbox/wa_since940) -- updated weekly
* [WA since 2015](https://rpubs.com/moxbox/wa_since2015) -- updated weekly
* [WA since 2000](https://rpubs.com/moxbox/wa_since2000) -- updated annually, and uses only FE data because WaPo doesn't go back that far

*US* -- [US per capita trends by state](https://rpubs.com/moxbox/statepercapitatrends)

____

*Find a bug/error?*  Please report it as an issue, thx!

*Want to get involved?* Please contact the repository maintainer
