# README

## Authors
Wendy Shi, Holt Cochran, Vika Li, Jiali Wang, Jasmine Jia

## Description
This research explores the intersection of COVID-19 mortality, political ideology, and racial/ethnic disparities in the United States. It examines state-level variations in COVID-19 policies, partisan influences, and their impact on public health outcomes, particularly for minority groups.

## Research Question
What is the relationship between political ideology, race, and the severity of COVID-19 impacts, and how do state-level policies and public health responses shape these outcomes?

## Input Data
1. **us_election_2020.csv**: 2020 Presidential election results by state .
2. **covid-cases.csv**: CDC data containing weekly.
3. **Single-Race Population Estimates 2020-2022 by State and Single-Year Age-2.txt**:This dataset is for population composition of race and ethnicity
4. **mask mandate.csv**: CDC dataset on mask policies by county and state.
5.**Provisional_COVID-19_Deaths_by_County__and_Race_and_Hispanic_Origin_20241120.csv**: Contains COVID-19 death rate with race and ethnicity data.
6. The other datasets are subsets of the above data sets that are used in RShiny

## Graphs & Output
- Data processing and visualization scripts produce:
  - U.S.Maps showing COVID-19 deaths per county, mask mandate scatter plot can be found in folder “Vika_Code” under “maskeffect.Rmd” file and “deathratebycounty.Rmd”
  - Scatter plots linking death rates to 2020 election results, this can be found in folder “Hold_code” under “covid_deaths_vs_election2020.Rmd”.
  - Bubble plots analyzing political ideology and death counts, this can be found in the folder “Jiali_code” under “Bubbleplot_code. ipynb”.
  - Time series plots and of monthly death rates by state, and the boxplot can be found in the folder “Jasmine_code”under “ dataviz_Final_Jia.ipynb” .
  - Interactive plots comparing death rates among racial/ethnic groups can be found in the folder “Wendy_code” in the file “Wendy_Code.qmd”


## Presentation & Report
- The findings are summarized in a detailed report, which you can find here (https://docs.google.com/document/d/1sdf0uAUGU3qT1rTJTbS9N_JXr8w6dl2r0HN4h-Lyclg/edit?usp=sharing) supplemented with visualizations that highlight key trends and disparities. The dashboard offers interactive elements to explore data further, which you can in the folder “Shiny_Dec8_0126”. However, the the first and the last plots are having trouble rendering at the same time because of the file size,  so we recommend downloading the entire R file locally and run the RShiny on your local machine.

## Data Source References
“CDC WONDER: Single-race single-year population estimates.” Centers for Disease Control and Prevention, 23 Aub. 2023, Retrieved from https://wonder.cdc.gov/Single-Race-single-year-v2022.HTML
“Covid-19 Mortality by State.” Centers for Disease Control and Prevention, Centers for Disease Control and Prevention, 15 Feb. 2023, Retrieved from www.cdc.gov/nchs/pressroom/sosmap/covid19_mortality_final/COVID19.htm. 
“2020 US Presidential Election Results by State.” Kaggle, Associated Press, 5 Feb. 2021, Retrieved from www.kaggle.com/datasets/callummacpherson14/2020-us-presidential-election-results-by-state. 
“USA Statewise Latest COVID-19 Data.” Kaggle, Anandhuh, 5 March 2021, Retrieved from https://www.kaggle.com/datasets/anandhuh/usa-statewise-latest-covid19-data. 
"U.S. State and Territorial Public Mask Mandates From April 10, 2020 through August 15, 2021." Centers for Disease Control and Prevention, 15 August 2021, Retrieved from https://data.cdc.gov/Policy-Surveillance/U-S-State-and-Territorial-Public-Mask-Mandates-Fro/62d6-pm5i.
"Weekly United States COVID-19 Cases and Deaths by State." Centers for Disease Control and Prevention, Retrieved from https://data.cdc.gov/dataset/Weekly-United-States-COVID-19-Cases-and-Deaths-by-/yviw-z6j5.

