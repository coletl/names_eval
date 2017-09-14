# Inferring ethnicity given names: preprocessing and evaluation

This repository documents efforts to revise the code used in 
"What's in a Name? A Method for Extracting Information about Ethnicity from Names" 
([Harris 2015](http://www.jandrewharris.org/research/)).

Disggregated data on race and ethnicity are at times difficult or impossible for researchers to obtain.
Harris 2015 presents a novel method of linking individuals' names to their group identity via geographic location, 
yielding accurate, high-resolution estimates of the proportion of individuals in each group. 

Here, we re-evaluate coding decisions to identify which data and preprocessing steps are best and which are irrelevant.
To do this, we construct five false profiles: 
user-defined distributions of several Kenyan ethnicities that will act as "true values" to estimate.
We then run the estimation function while varying parameter options to produce a data set of 360 distribution estimates.
Regressing our error — the discrepancy between each estimate and the "true value" defined in the profiles — 
on the parameter combinations allows us to identify the parameter options that were most salient.
This approach also affords us some clues as to the variation in performance across potential population distributions 
and our accuracy for each ethnicity.

Our tests yielded a few conclusions. In data from Kenyan voter registers,
1. preprocessing/cleaning methods used to build the conditional probability matrix should match the steps used to clean the input names;
2. the best cleaning method was to remove only first names, retaining middle and last names;
3. using older names (roughly five years) to build the conditional probabilities had little effect on performance;
4. our two functions that implement the estimations yielded similar results.

## You may find the code in this repository helpful if . . .
- You would like to estimate disaggregated data on group identity (e.g., the percent of a polling station's voters that are Luo).
- You already have disaggregated data that may be used to infer group inclusion (e.g., names of voters).
- You already have (aggregated or disaggragated) data on group distributions (e.g., the percent of some area that is Luo).
- You can link your disaggregated group-inclusion data to your demographic distributions (e.g., determine which areas have more Luo names than others).
- AND there are areas that are homogenous with respect to each of the groups in which you are interested (e.g., some areas are 90% Luo).
