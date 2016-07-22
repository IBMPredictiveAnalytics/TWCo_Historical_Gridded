# Extension for The Weather Company Weather

# Overview
This extension provide capacity to fetch weather observation data through The Weather Company's Historical Observations API. Beside API, this extension can
1. Pull in any historical weather date range (exceeds the API 30-31 day limit)
2. Allow multiple lat / long locations input

# Overview for Standard Site-Based Historical Observations
The Historical Observations API will return past weather observations, up to 1 month (31 days maximum) between the start and end dates parameters. The observations are from METAR and SYNOP reporting stations. Reporting stations report weather observations at different intervals. Some observations are reported hourly, some every 3 hours, every 6 hours, etc. Some stations do not report during nighttime hours. The reporting times per day for each reporting station can also vary. Therefore, this API will simply return all reported weather observations received for a given location and day.
Standard Site-Based Historical Observations

# Notes
1. Because of Modeler issues, "lat" can't show in dailog.
2. All input type(lat, long, startdate, enddate) should be string. 

# Reference
[The Weather Company](http://www.theweathercompany.com/)
[Historical Weather](http://goo.gl/DplOKj)

---
Requirements
----
- IBM SPSS Modeler v18
- R Essentials for SPSS Modeler plugin 

More information here: [IBM Predictive Extensions][2]

---
Installation instructions
----
1. Download the extension.
2. Install extension from [gallery][2] 

---
Examples stream and data
----
Example folder include example stream and sample data

---
License
----

[Apache 2.0][1]


Contributors
----
- Yu Wenpei [(mail)](yuwenp@cn.ibm.com)

[1]:http://www.apache.org/licenses/LICENSE-2.0.html
[2]:https://developer.ibm.com/predictiveanalytics/downloads/#tab2
