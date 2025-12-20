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

While trying to convert AZ_long to a shapefile I ran into some trouble: there are 3224 collections with spatial coordinates. Attempting to fix now (~11:20am)

Noon: regex doesn't help here (although I tried with the code below). Going to go back to the original dataset for 2020-2024 since that one has intersection data at least (there is nothing we can do for the years prior; we'll be missing out on 4760 females and 3 positive pools--1 for SLE and 2 for WNV)
Never mind. There is no intersection data for 20-24. Gonna just waste it all unforunately.