# Vector-Cleaning

Prelim data processing

### Preamble

This (work in progress) repo will include all scripts needed to clean state-specific vector control data. Due to the fact that the data provided includes PII, the files themselves can't/won't be uploaded to this repo.

## Script descriptions

### Sweep and Mop

The primary cleaning file, 'Sweep and Mop.R', is broken up into two sections. One for abundance datasheet cleaning, and the other for pools (*as of December 2025, only CA and AZ have provided pool data*).

The 'sweep' function is a function that takes a raw, potentially messy .csv, .xlsx, or .xls file and both cleans and standardizes species collection and geospatial information. The following table describes the functions arguments.

| Argument | Description |
|---------------------|---------------------------------------------------|
| path | Character. The directory of a single datasheet. Defaults to NULL. |
| state_name | Character. Abbreviated state (e.g. "TX", "WA") that vector data comes from. This arg is used to create a 'state' column--useful when binding multiple agencies' data. |
| extensions | If working with multi-year data that is spread across multiple files, a list of path directories. Defaults to NULL. |
| sheets | TRUE or FALSE. Indicates whether data is stored within multiple excel sheets. Defaults to FALSE. |
| dirs | Street direction dictionary (used in the purposes of standardizing addresses using 'postmastr': see pm_dictionary) |
| type | The type of datasheet to be cleaned: either 'abundance' or 'pool'. Defaults to 'abundance'. |

Note that the 'sweep' function has the following package dependencies (all of which are loaded in the script itself): 'janitor', 'parzer', 'tidygeocoder', 'postmastr', and 'tigris'. The script itself uses the CA and Nueces & Dallas county datasets as proof of concepts.

The 'sweep' function can be split into a few sections and accomplishes multiple pre-processing tasks that don't require manual oversight:

1)  Standardizing the genus representation for *Aedes* (Ae), *Culex* (Cu), *Anopheles* (An), and *Psorophora* (P); ensuring that different agency standards for species ID'ing inputs don't impact statistical analyses.
2)  For agencies that record street addresses of trap locations (as opposed to giving traps unique identifiers), address representation is standardized to *HOUSE NUMBER* *STREET NAME* *STREET SUFFIX*

3a) Parse geoocoordinates to ensure they are in degree decimal format (via 'parzer')

3b) Geocode addresses with missing or obviously incorrect geocoordinates (outside of the county's bounding box via 'tigris'--an imperfect but reasonable solution)

4)  Subset data to only include female collections

**Thus, this function has the following limitations and assumptions (specifically with respect to the data input format)**.

### Abundance information cleaning
---
1)  The input data **must** have at least the following variables/column names (all other variables are ignored--for now, anyway):

| Variable | Description |
|----------------------------|--------------------------------------------|
| county | County of collection |
| sampled_date | mm/dd/yyyy trap was collected |
| address | Street OR identifier of trap placement (e.g. "Hc11", "14", etc.) |
| collection_method | Trap type of collection |
| latitude | Coordinate. Can be of any format initially. |
| longitude | Coordinate. Can be of any format initially. |
| mosquito_id | Species collected on sampled_date |
| number_of_mosquitoes | Number of females of a particular species collected on sampled_date |

2)  It is assumed that the 'tigris' package has the most up-to-date and correct county boundaries as per its counties() function call. Tigris uses the Census line files to draw these county boundaries.
3)  Some trap locations/geocoordinates may simply be impossible to decipher/clean using the combination of 'parzer', 'tigris', and 'postmastr' leveraged in this function. This can cause issues in geospatial analysis if not caught.

3b) **It is imperative that all data cleaned using this function is turned into a shapefile and plotted prior to analysis to ensure that there are no error in spatial coordinates**.

3c) As a corollary to this: the 'sweep' function is just that--a function to perform the basic pre-processing needed to get your hands on somewhat-usable-not-totally-useless vector data. The 'mop' portion (work in progress)--which is intended to tackle the more complex and minute errors/issues--is a process that'll look different from dataset to dataset.

### Pools information cleaning
---

The procedure is identical to the one described above except for the following changes to the required input data structure (marked in bold):

| Variable | Description |
|----------------------------|--------------------------------------------|
| county | County of collection |
| sampled_date | mm/dd/yyyy trap was collected |
| address | Street OR identifier of trap placement (e.g. "Hc11", "14", etc.) |
| collection_method | Trap type of collection |
| latitude | Coordinate. Can be of any format initially. |
| longitude | Coordinate. Can be of any format initially. |
| mosquito_id | Species collected on sampled_date |
| number_of_mosquitoes | Number of a particular species collected on sampled_date (FEMALES) |
| disease | The disease being tested for. |
| result | Result of test (1 if positive, else 0). |