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