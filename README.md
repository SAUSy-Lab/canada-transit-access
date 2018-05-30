
## Mapping Access to Employment in Canadian Cities

Code for measuring and visualizing access to employment in Canadian cities.

View the data on an interactive map [here](https://sausy-lab.github.io/canada-transit-access/map.html) :)

The raw data is [here](https://github.com/SAUSy-Lab/canada-transit-access/tree/gh-pages/data). A brief description is below, a more detailed description is in a [technical report](https://sausy-lab.github.io/canada-transit-access/data/canada-access-to-jobs-tech-report.pdf), slides from a recent presentation are [here](https://github.com/SAUSy-Lab/canada-transit-access/blob/master/slides/UAA%20AAG%202018.pdf)


-----------------------------

Measures of access to employment were generated based on the following formulation:

![A1](imgs/a1.png)

Ai is the accessibility measure for location i, Oj is the number of jobs at a location j, and t is the travel time from i to j. For the first three measures on the map, f(t) is simply a binary measure (0 or 1) of whether the travel time is greater or less than 30, 45, or 60 minutes respectively, in order to count the number of jobs reachable within these thresholds. For the accessibility index using a gravity model, f(t) is an inverse-decay function parameterized so that f(t) = 1 for t = 0, f(t) = 0.5 for t = 30 minutes, decaying to f(t) = 0 for t = 90 minutes.

The competitive measure accounts for the size of the labour force who are competing for jobs by discounting Oj by the labour force within its catchment area.

![A2](imgs/a2.png)

Lj is a measure of access to the labour force from j and Pi is the size of the labour force at i. This approach requires iteration until convergence. This is further expanded upon to account for a multi-modal labour force, ability to compare between regions, and any imbalance between the number of jobs and number of workers within a region. Lambda is a travel mode (e.g. car or transit) in the following.

![A3](imgs/a3.png)  
![A4](imgs/a4.png)

Data for the location of jobs and the labour force are from Statistics Canada's 2016 census of population. The network graphs for measuring travel times from i to j were computed via OpenTripPlanner and OSRM using input data from OpenStreetMap and GTFS data from various transit agencies across Canada. The travel times for public transit and by car are for the morning commute period (for transit, this was computed for every minute from 7:00am to 9:00am and then averaged to account for fluctuating schedules). The overlay dot density layers are also from the 2016 census.

The code to generate the accessibility measures in this map are broken down into the following sub-folders.

**computing-travel-times** - For computing origin-destination matrices between home locations and potential work locations using OpenTripPlanner and OSRM

**computing-accessibility-metrics** - Code for inputting travel times and demographic and employment data from the Canadian census to generate measures of access to employment by travel mode.

**analysis-and-plots** - Various R scripts for tabulating, correlating, and visualizing the accessibility measures alongside demographic and socio-econmoic data.

**

Any comments or questions, contact me at jeff.allen AT utoronto.ca. If the data are used for research purposes, please cite as

```
@techreport{allen2018,
  Title    = {Generating measures of access to employment for Canada’s eight largest urban regions},
  Author   = {Allen, Jeff and Farber, Steven},
  Year     = {2018}
}
```
