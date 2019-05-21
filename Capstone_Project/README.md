---
title: "Proposal for Springboard IDS Capstone Project"
author: "J Slaga"
date: "3/14/2019"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
## Predicting the Likelihood of Domestic Flight Delays

### Problem

Whether traveling for business or pleasure, most travelers want to minimize their travel time in order to better utilize their time. A flight delay can not only be bothersome, but often starts a snowball effect of other troubles such as missed connector flights or late hotel check-ins. Are certain locations, times, days of week, or carriers subject to more delays than others? Knowing which flights have a higher likelihood of being delayed, one could preemptively alter their travel plans to avoid significant impediments. I will examine US domestic flight data for 2015 to find which factors most affect flight departures and create a model predicting the likelihood of delayed flights.


### Client 

Companies booking business travel can use this information to more efficeintly book travel for their employees by minimizing time lost to delays. Travel booking sites such as Expedia or Kayak could use such a model to flag "high risk" flights providing their users a premium decision making tool for their bookings. These companies could extend the service to an app to be used by travel agents or frequent travelers.

### Data

Data was found on Kaggle: https://www.kaggle.com/usdot/flight-delays. Original data was collected by and published on the Department of Transportation's Bureau of Transportation Statistics.

This will focus primarily on variables pertaining to departure delays from the 2015 data set:

* Origin Airport
* Airline
* Delay reason: weather, air system, security, airline, or late aircraft
* Day of week

### Deliverables

* Code
* Slide deck
