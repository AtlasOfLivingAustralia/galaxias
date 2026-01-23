# Standardise an Event dataset

In a research project, data collection can take place at multiple
locations and times. At each location and time, there often multiple
collected samples to capture variation in a study area or time-period.
In Darwin Core, the data collected from this type of project is
*Event*-based.

*Events* are any action that “occurs at some location during some time.”
([from TDWG](https://dwc.tdwg.org/list/#dwc_Event)). Each sample, for
example, is a unique event, with its own environmental attributes (like
topography, tree cover and soil composition) that affect what organisms
occur there and how likely they are to occur. Observations of organisms
take place *within* each Event. As such, Events add hierarchy to a
dataset by grouping simultaneous observations into groups, as opposed to
Occurrence-only data which is processed as if all occurrences are
independent. Event-based data collection adds richness to ecological
data that can be useful for more advanced modelling techniques.

Here we will demonstrate an example of how to convert Event-based data
to Darwin Core standard. To do so, we will create two csv files,
events.csv and occurrences.csv, to build a Darwin Core Archive.

## The dataset

For this example, we’ll use a dataset of frog observations from a [2015
paper in PLOS ONE](https://doi.org/10.1371/journal.pone.0140973). Data
were collected by volunteers using 5-minute audio surveys, where each
row documents whether each frog species was detected over that 5-minute
recording, recorded as present (`1`) or absent (`0`). For the purpose of
this vignette, we have downloaded the source data from
[Dryad](https://doi.org/10.5061/dryad.75s51), reduced the number of
rows, and converted the original excel spreadsheet to three `.csv`
files: `sites`, `observations` and `species list`.

### Sites

The `sites` spreadsheet contains columns that describe each survey
location (e.g. `depth`, `water_type`, `latitude`, `longitude`) and
overall presence/absence of each frog species in a site (e.g. `cpar`,
`csig`, `limdum`). We won’t use the aggregated species data stored
here - we’ll instead export the raw observations - but we’ll still
import the data, because it’s the only place that spatial information
are stored.

``` r
library(readr)
library(readr)
library(dplyr)
library(tidyr)
sites <- read_csv("events_sites.csv")

sites |> rmarkdown::paged_table()
```

### Observations

The `observations` spreadsheet contains columns that describe the
sample’s physical properties (e.g. `water_type`, `veg_canopy`), linked
to `sites` by the `site_code` column. More importantly, it records
whether each species in the region was recorded during that particular
survey (e.g. `cpar`, `csig`, `limdum`).

``` r
obs <- read_csv("events_observations.csv")

obs |> rmarkdown::paged_table()
```

### Species list

Finally, the `species list` spreadsheet lists the eight frog species
recorded in this dataset, and the `abbreviation` column contains the
abbreviated column name used in the `observations` dataset.

``` r
species <- read_csv("events_species.csv")

species
#> # A tibble: 8 × 3
#>   scientific_name            common_name            abbreviation
#>   <chr>                      <chr>                  <chr>       
#> 1 Crinia parinsignifera      Plains Froglet         cpar        
#> 2 Crinia signifera           Common Eastern Froglet csig        
#> 3 Limnodynastes dumerilii    Pobblebonk             limdum      
#> 4 Limnodynastes peronii      Striped Marsh Frog     limper      
#> 5 Limnodynastes tasmaniensis Spotted Grass Frog     limtas      
#> 6 Litoria peronii            Emerald Spotted Frog   lper        
#> 7 Litoria verreauxii         Alpine Tree Frog       lver        
#> 8 Uperoleia laevigata        Smooth Toadlet         ulae
```

## Prepare `events.csv`

As the `observations` spreadsheet is organised at the sample-level,
where each row contains multiple observations in one 5-minute audio
recording, we can create an *Event*-based dataframe at the sample-level
to use as our `events.csv`.

First, let’s assign a unique identifier `eventID` to data, which is a
requirement of Darwin Core Standard. Using
[`set_events()`](https://rdrr.io/pkg/corella/man/set_events.html) and
[`composite_id()`](https://rdrr.io/pkg/corella/man/identifier_functions.html),
we can create a new column `eventID` containing a unique ID constructed
several types of information in our dataframe.

``` r
obs_id <- obs |>
  select(site_code, year, any_of(species$abbreviation)) |>
  set_events(
    eventID = composite_id(sequential_id(), site_code, year)
    ) |>
  relocate(eventID, .before = 1) # re-position
#> ⠙ Checking 1 column: eventID
#> ✔ Checking 1 column: eventID [314ms]
#> 

obs_id
#> # A tibble: 123 × 11
#>    eventID    site_code  year  cpar  csig limdum limper limtas  lper  lver  ulae
#>    <chr>      <chr>     <dbl> <dbl> <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>
#>  1 0001-AMA1… AMA100     2004     1     0      0      0      1     1     0     0
#>  2 0002-AMA1… AMA100     2007     1     0      1      0      1     0     0     0
#>  3 0003-AMA1… AMA100     2007     1     0      1      0      1     0     0     0
#>  4 0004-AMA1… AMA100     2005     1     1      1      0      1     0     0     0
#>  5 0005-AMA1… AMA100     2008     1     0      1      0      0     1     0     0
#>  6 0006-AMA1… AMA100     2008     1     0      1      0      1     1     0     0
#>  7 0007-AMA1… AMA100     2013     1     0      1      0      1     0     0     0
#>  8 0008-AMA1… AMA100     2008     1     0      1      0      1     1     0     0
#>  9 0009-AMA1… AMA100     2013     1     1      0      0      0     0     0     0
#> 10 0010-AMA1… AMA100     2014     1     1      1      0      1     0     0     0
#> # ℹ 113 more rows
```

Next we’ll add site information from the `sites` spreadsheet. Then we
use
[`set_coordinates()`](https://rdrr.io/pkg/corella/man/set_coordinates.html)
to assign our existing columns to use valid Darwin Core Standard column
names, and add 2 other required columns `geodeticDatum` and
`coordinateUncertaintyInMetres`.

``` r
obs_id_site <- obs_id |>
  left_join(
    select(sites, site_code, latitude, longitude),
    join_by(site_code)
    ) |>
  set_coordinates(
    decimalLatitude = latitude, 
    decimalLongitude = longitude,
    geodeticDatum = "WGS84",
    coordinateUncertaintyInMeters = 30
    ) |>
  relocate(decimalLatitude, decimalLongitude, .after = eventID) # re-position cols
#> ⠙ Checking 4 columns: coordinateUncertaintyInMeters, decimalLatitude, decimalLo…
#> ⠹ Checking 4 columns: coordinateUncertaintyInMeters, decimalLatitude, decimalLo…
#> ✔ Checking 4 columns: coordinateUncertaintyInMeters, decimalLatitude, decimalLo…
#> 

obs_id_site
#> # A tibble: 123 × 15
#>    eventID   decimalLatitude decimalLongitude site_code  year  cpar  csig limdum
#>    <chr>               <dbl>            <dbl> <chr>     <dbl> <dbl> <dbl>  <dbl>
#>  1 0001-AMA…           -35.2             149. AMA100     2004     1     0      0
#>  2 0002-AMA…           -35.2             149. AMA100     2007     1     0      1
#>  3 0003-AMA…           -35.2             149. AMA100     2007     1     0      1
#>  4 0004-AMA…           -35.2             149. AMA100     2005     1     1      1
#>  5 0005-AMA…           -35.2             149. AMA100     2008     1     0      1
#>  6 0006-AMA…           -35.2             149. AMA100     2008     1     0      1
#>  7 0007-AMA…           -35.2             149. AMA100     2013     1     0      1
#>  8 0008-AMA…           -35.2             149. AMA100     2008     1     0      1
#>  9 0009-AMA…           -35.2             149. AMA100     2013     1     1      0
#> 10 0010-AMA…           -35.2             149. AMA100     2014     1     1      1
#> # ℹ 113 more rows
#> # ℹ 7 more variables: limper <dbl>, limtas <dbl>, lper <dbl>, lver <dbl>,
#> #   ulae <dbl>, coordinateUncertaintyInMeters <dbl>, geodeticDatum <chr>
```

We now have a dataframe with sampling and site information, organised at
the sample-level. Our final step is to reduce `obs_id_site` to only
include columns with valid column names in Event-based datasets. This
drops the frog species columns from our dataframe.

``` r
events <- obs_id_site |>
  select(
    any_of(event_terms())
    )

events
#> # A tibble: 123 × 6
#>    eventID           year decimalLatitude decimalLongitude geodeticDatum
#>    <chr>            <dbl>           <dbl>            <dbl> <chr>        
#>  1 0001-AMA100-2004  2004           -35.2             149. WGS84        
#>  2 0002-AMA100-2007  2007           -35.2             149. WGS84        
#>  3 0003-AMA100-2007  2007           -35.2             149. WGS84        
#>  4 0004-AMA100-2005  2005           -35.2             149. WGS84        
#>  5 0005-AMA100-2008  2008           -35.2             149. WGS84        
#>  6 0006-AMA100-2008  2008           -35.2             149. WGS84        
#>  7 0007-AMA100-2013  2013           -35.2             149. WGS84        
#>  8 0008-AMA100-2008  2008           -35.2             149. WGS84        
#>  9 0009-AMA100-2013  2013           -35.2             149. WGS84        
#> 10 0010-AMA100-2014  2014           -35.2             149. WGS84        
#> # ℹ 113 more rows
#> # ℹ 1 more variable: coordinateUncertaintyInMeters <dbl>
```

We can specify that we wish to use `events` in our Darwin Core Archive
with
[`use_data()`](https://galaxias.ala.org.au/R/reference/use_data.md),
which will save `events` as a csv file in the default directory
`data-publish` as `./data-publish/events.csv`.

``` r
events |> use_data()
```

## Prepare `occurrences.csv`

Let’s return to `obs_id_site`, which contains an `eventID` and site
information for each sample. To create an **Occurrence**-based dataframe
that conforms to Darwin Core Standard, we will need to transpose this
wide-format dataframe to *long* format, where each row contains one
observation. We’ll select the `eventID` and abbreviated species columns,
then pivot our data so that each species observation is under
`abbreviation` and each presence/absence recorded under `presence`.

``` r
obs_long <- obs_id_site |>
  select(eventID, any_of(species$abbreviation)) |>
  pivot_longer(cols = species$abbreviation,
               names_to = "abbreviation",
               values_to = "presence")

obs_long
#> # A tibble: 984 × 3
#>    eventID          abbreviation presence
#>    <chr>            <chr>           <dbl>
#>  1 0001-AMA100-2004 cpar                1
#>  2 0001-AMA100-2004 csig                0
#>  3 0001-AMA100-2004 limdum              0
#>  4 0001-AMA100-2004 limper              0
#>  5 0001-AMA100-2004 limtas              1
#>  6 0001-AMA100-2004 lper                1
#>  7 0001-AMA100-2004 lver                0
#>  8 0001-AMA100-2004 ulae                0
#>  9 0002-AMA100-2007 cpar                1
#> 10 0002-AMA100-2007 csig                0
#> # ℹ 974 more rows
```

Now we’ll merge the correct names to our frog species by joining
`species` with `obs_long`.

``` r
obs_long <- obs_long |>
  left_join(species, join_by(abbreviation), keep = FALSE) |>
  relocate(presence, .after = last_col()) # re-position column
```

Now we can reformat our data to use valid Darwin Core column names using
`set_` functions. Importantly, Darwin Core Standard requires that we add
a unique `occurrenceID` and the type of observation in the column
`basisOfRecord`.

``` r
obs_long_dwc <- obs_long |>
 set_occurrences(
   occurrenceID = composite_id(eventID, sequential_id()),
   basisOfRecord = "humanObservation",
   occurrenceStatus = dplyr::case_when(presence == 1 ~ "present",
                                       .default = "absent")
   ) |>
 set_scientific_name(
   scientificName = scientific_name
   ) |>
 set_taxonomy(
   vernacularName = common_name
   )
#> ⠙ Checking 3 columns: occurrenceID, basisOfRecord, and occurrenceStatus
#> ✔ Checking 3 columns: occurrenceID, basisOfRecord, and occurrenceStatus [923ms]
#> 
#> ⠙ Checking 1 column: scientificName
#> ✔ Checking 1 column: scientificName [311ms]
#> 
#> ⠙ Checking 1 column: vernacularName
#> ✔ Checking 1 column: vernacularName [335ms]
#> 

obs_long_dwc
#> # A tibble: 984 × 7
#>    eventID          abbreviation occurrenceID     basisOfRecord occurrenceStatus
#>    <chr>            <chr>        <chr>            <chr>         <chr>           
#>  1 0001-AMA100-2004 cpar         0001-AMA100-200… humanObserva… present         
#>  2 0001-AMA100-2004 csig         0001-AMA100-200… humanObserva… absent          
#>  3 0001-AMA100-2004 limdum       0001-AMA100-200… humanObserva… absent          
#>  4 0001-AMA100-2004 limper       0001-AMA100-200… humanObserva… absent          
#>  5 0001-AMA100-2004 limtas       0001-AMA100-200… humanObserva… present         
#>  6 0001-AMA100-2004 lper         0001-AMA100-200… humanObserva… present         
#>  7 0001-AMA100-2004 lver         0001-AMA100-200… humanObserva… absent          
#>  8 0001-AMA100-2004 ulae         0001-AMA100-200… humanObserva… absent          
#>  9 0002-AMA100-2007 cpar         0002-AMA100-200… humanObserva… present         
#> 10 0002-AMA100-2007 csig         0002-AMA100-200… humanObserva… absent          
#> # ℹ 974 more rows
#> # ℹ 2 more variables: scientificName <chr>, vernacularName <chr>
```

We now have a dataframe with observations organised at the
occurrence-level. Our final step is to reduce `obs_long_dwc` to only
include columns with valid column names in Occurrence-based datasets.
This drops the `abbreviation` column from our dataframe.

``` r
occurrences <- obs_long_dwc |>
  select(
    any_of(occurrence_terms())
    )
```

We can specify that we wish to use `occurrences` in our Darwin Core
Archive with
[`use_data()`](https://galaxias.ala.org.au/R/reference/use_data.md),
which will save `occurrences` as a csv file in the default directory
`data-publish` as `./data-publish/occurrences.csv`.

``` r
occurrences |> use_data()
```

In data terms, that’s it! Don’t forget to add your metadata using
[`use_metadata_template()`](https://delma.ala.org.au/R/reference/use_metadata_template.html)
and
[`use_metadata()`](https://galaxias.ala.org.au/R/reference/use_metadata.md)
before you build and submit your archive.

## Summary

The hierarchical structure of Event-based data (ie Site -\> Sample -\>
Occurrence) adds richness, allowing for information like repeated
sampling and presence/absence information to be preserved. This richness
can enable more nuanced probabilistic analyses like species distribution
models or occupancy models. We encourage users with Event-based data to
use galaxias to standardise their data for publication and sharing.
