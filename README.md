# Mental Health in Tech dashboard

Mental health awareness has been on the public's agendas over the last years. Tech is an area of increasing interest regarding mental heath because of the 
long hours of working and their insititutional culture. This dashboard explores different data associations possible with the data.

The dashboard is published here: https://bridgettesullivan.shinyapps.io/Mental_Health_In_Tech_Data_Viz_Proj/ 

## Dashboard
The dashboard has data on the characteristics of the workers and the companies they work for, insights from the US workers by state and a sentiment analysis 
of the comments left by the respondents. This were the steps and general ideas taken in the dashboard:

### Data Cleaning
After obtaining the data, the first necessary step was to clean the data. Some variables had values that were impossible, like negative values in the Age column or values above 200 and below 17, that were updated to NA values. Additionally, the gender variable appeared to be a text entry column rather than a selection, so there are many different values with spelling errors and different capitalizations that had to be updated to accurately reflect gender groupings.


### Creating Age Bins
For ease of filtering in the visualizations, we created a binned measure for age. This allows the visualizations to be presented in the context of a range of ages rather than having to look at one specific age when interacting with the graphics.

### Ordinal Variable Conversion
Most of the responses in this survey were either boolean or ordinal in nature. The ordinal variables included choices like “Often”, “Sometimes”, “Rarely”, and “Never” to communicate the respondent's sentiments around their workplace and their own mental health. These ordinal variables were converted to a numerical scale that corresponded with the ordering of the choices. Converting these variables allows for summary statistics, like averages, to be calculated across different geographies and other groups.

### Averages versus counts
Averages were taken whenever there were continuous variables and counts for all the categorical variables.

### Visualization Methodology
We created visualizations for this data set with the following goals in mind:
Understand the general distribution of responses
Identify relationships between variables
Visualize differences in responses geographically
Process textual commentary to understand important themes

Understanding these areas will assist in developing a policy recommendation surrounding mental health in the tech industry.

## Data
This project uses data from the Mental Health in Tech Survey took in 2014 retireved from Kaggle https://www.kaggle.com/datasets/osmi/mental-health-in-tech-survey.

The cleaned survey data is included in this repo.

## Code
The code for this is centered in the Shiny app MentalHealthInTech.R
However, each of the tabs is also reported in Archive for easier reproducibility.

