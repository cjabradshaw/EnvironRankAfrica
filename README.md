# Environmental Ranks of African Countries
<img align="right" src="AfricaENV.png" alt="Africa Environmental Index" width="200" style="margin-top: 20px">

R code and data for structural equation models and boosted regression trees to estimate relationships between environmental degradation and socio-economic variables among African nations

code updated May 2019

R code accompanies the following paper:

Bradshaw, CJA, E Di Minin. 2019. <a href="http://doi.org/10.1038/s41598-019-45762-3">Socio-economic predictors of environmental performance among African nations</a>. <em>Scientific Reports</em> 9: 9306. doi:10.1038/s41598-019-45762-3

## Abstract
Socio-economic changes in Africa have increased pressure on the continent’s ecosystems. Most research investigating environmental change has focused on the changing status of specific species or communities and protected areas, but has largely neglected the broad-scale socio-economic conditions underlying environmental degradation. We tested national-scale hypotheses regarding the socioeconomic predictors of ecosystem change and degradation across Africa, hypothesizing that human
density and economic development increase the likelihood of cumulative environmental damage. Our combined environmental performance rank includes national ecological footprint, proportional species threat, recent deforestation, freshwater removal, livestock density, cropland coverage, and per capita emissions. Countries like Central African Republic, Botswana, Namibia, and Congo have the best relative environmental performance overall. Structural equation models indicate that increasing population density and overall economic activity (per capita gross domestic product corrected for purchasing-power parity) are the most strongly correlated with greater environmental degradation, while greater wealth inequality (Gini index) correlates with better environmental performance. This represents the first Africa-scale assessment of the socio-economic correlates of environmental degradation, and suggests that dedicated family planning to reduce population growth, and economic development that limits agricultural expansion (cf. intensification) are needed to support environmental sustainability.

BY: <a href="https://scholar.google.com.au/citations?hl=en&user=1sO0O3wAAAAJ&view_op=list_works&sortby=pubdate">Corey J.A. Bradshaw</a> (Flinders University, Australia) & <a href="https://scholar.google.com.au/citations?hl=en&user=rztzLosAAAAJ">Enrico Di Minin</a> (University of Helsinki, Finland)

CONTACT: corey.bradshaw@flinders.edu.au
<br>
<a href="http://globalecologyflinders.com">globalecologyflinders.com</a>

## Data files
All 28 .csv data files (stored in the '<a href="https://github.com/cjabradshaw/EnvironRankAfrica/tree/master/data">data</a>' sub-directory) are included to run code. Update working directory accordingly.

## Requires the following libraries
- <code>boot</code>
- <code>lme4</code>
- <code>sem</code>
- <code>semGOF</code> (note: might not function in later versions of R)
- <code>dismo</code>
- <code>gbm</code>

