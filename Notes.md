# Arizona

### December 16, 2025

Two things to initially note.

First, there are some discrepancies that we can easily take care of w/r/t column names and object types (I'm going to just immediately subset the data into relevant columns):

| 2013-2019 name (type; format)  | 2020-2024 name (type; format)    |
|------------------------|--------------------------|
| ID Number (chr)        | TrapID (chr)             |
| Lab Date (chr; m/d/yy) | Lab_Date (chr; mm/dd/yy) |
| Test Result (num; 0/1) | Result (num; 0/1)        |
| NA                     | Test Date                |

We can subset the data to include the following variables in the following (type; format):

| Variable  | (type; format)   |
|-----------|------------------|
| TrapID        | chr             |
| Species      | chr |
| Males | int |
| Females | int |
| Disease | chr |
|Latitude | num |
|Longitude | num|
| Lab_Date  | (Date; mm/dd/yy) |
| Result | (int; 0/1)        |

### December 20, 2025

While trying to convert AZ_long to a shapefile I ran into some trouble: there are 1612 collections with spatial coordinates. Attempting to fix now (~11:20am)

Noon: regex doesn't help here (although I tried with the code below). Going to go back to the original dataset for 2020-2024 since that one has intersection data at least (there is nothing we can do for the years prior; we'll be missing out on 4760 females and 3 positive pools--1 for SLE and 2 for WNV)
Never mind. There is no intersection data for 20-24. Gonna just waste it all unforunately.

### December 21, 2025

Turned AZ into shapefile (both wide and long) and created some basic plots of abundance both spatial and otherwise.

One discrepancy I did notice that will need to be addressed are instances where a species is recorded as observed but there are no counts associated with it; e.g. this observation in 2013:

RT555
2013-04-04
Ae aegypti
0
0
NA
0
2013
Apr
0.0000000
0
POINT (-111.744 33.43114)

There are 23 of these observations (find them with the following command: AZ_sf %>% filter(Females == 0 & Males == 0 & !(Species %in% c("None", "Bird", "Unknown", "Cs species", 
                                                              "Ps species", "An species", NA, "None")))). I've removed them (0.003% of observations)
                                                              
I think for now I can move on to the Cali dataset.

### December 22, 2025

I will only be able to automate so much in terms of having a single function to clean datasets, but we can at least build something to do the 'janitor' level operations (CA and TX script)

# TX

This will be the biggest pain so I am pivoting to just do this instead. 

Column formats are not consistent across sheets, so we have to do that manually in the actual xlsx file.

What a [powerful tool](https://docs.ropensci.org/parzer/) I have found! This one package is able to parse through the insanely messy DFW dataset with no issues. Incredible stuff. This should let us be **much** less wasteful! Stoked about this. 

Taking a break for today since building out that function took some time amongst other distractions, haha.