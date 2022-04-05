# fewapo

This repository contains R language scripts for scraping, cleaning and merging the data on persons killed by police from the [Fatal Encounters Project](https://fatalencounters.org/) and the [Washington Post police shootings database](https://www.washingtonpost.com/graphics/investigations/police-shootings-database).

We are committed to accurate, transparent, reproducible research.  All of the tools used in our workflow are open-source and free to the public.

____

<p align="center">
  <b>If you use these data or scripts, please consider donating to Fatal Encounters at:  </b><a href="https://fatalencounters.org/donate/">https://fatalencounters.org/donate/</a>
</p>

____

## The `DataCleaningScripts` folder 

This contains the key files needed to construct the merged dataset and population counts:

* `ScrapeMerge` - the main scraping / cleaning / merging / datafile construction script
* `fixes_precleaning` & `fixes_postcleaning` - called by `ScrapeMerge` to spot fix errors identified in both datasets. The `postcleaning` corrections are applied just before the final merge.
* `StateCensusData` - script for scraping and managing census population estimates, used to calculate *per capita* rates for the US report

Running/sourcing the `ScrapeMerge` script will create the cleaned datafiles needed for the WA state reports below.

If you want to run the US national per capita analysis, you will also need to source the `StateCensusData` script

## The `Analyses` folder

This contains the scripts that use the cleaned/merged data to produce reports that are hosted on rpubs.com:

*Washington state*  
* [WA since the passage of Inititative-940](https://rpubs.com/moxbox/wa_since940) -- updated weekly
* [WA since 2015](https://rpubs.com/moxbox/wa_since2015) -- updated weekly
* [WA since 2000](https://rpubs.com/moxbox/wa_since2000) -- updated annually, and uses only FE data because WaPo doesn't go back that far

*US* -- [US per capita trends by state since 2000](https://rpubs.com/moxbox/statepercapitatrends) -- updated annually, also only use FE data given the time frame.

### `Common` subfolder

The `Common` subfolder includes a set of scripts/rmarkdown files that are used by the WA since 940 and WA since 2015 reports.  They are called by the rmarkdown files for those reports via either `source` or the chunk option `child=`.

### Note the yaml params

For the WA since 940 and 2015 reports, the format is essentially identical, only the data (and captioning) are different.  To facilitate maintenance, these small differences are captured in the yaml as `params`, and the `params` are referred to in the report markdown. 

____

**NOTE:**  The Fatal Encounters project has temporarily paused updating, since the end of calendar year 2021.  During this time we are keeping track of the incidents in WA State (only).  The new cases can be found in the `temp_newnames.csv` file.  WaPo updates are integrated as usual.

____

*Find a bug/error?*  Please report it as an issue, thx!

*Want to get involved?* Please contact the repository maintainer
