# Final Project Proposal

![](https://recruitingtimes.org/wp-content/uploads/2016/11/Gender-Pay-Gap.png)

##### Copyright Recruiting Times


### Author:
- Jason Li
- John Zhou
- Colson Xu
- Matthew Cho


### Project Description
- Our group will use the statistics of enrollment of each major in universities. Institute of Education Sciences gathered these statistics through surveys across
United States. The website of National Center for Education Statistics allowed me to gain access
for the data. The [**database**](https://nces.ed.gov/programs/digest/) we are using includes education information starting from 2012 to current year. More specifically, we will be looking at statistics about genders, degree types, study areas, and ethnicities. 

- This data set can be beneficial for many people to view. However, our target audience are females because they may feel discriminated due to their gender. They might believe that there are gender salary gaps. With the data that our team analyzes, we can compare different years of salaries for each college major. Then we will analyze major choices that males and females tend to make. Using this dataset, we can help our target audience discover the real reason behind gender salary gap. 

- The audience of this project will, _hopefully_, learn:
  1. gender and salary gaps armong different majors.
  2. the cause of gender gaps.
  3. gender difference in different majors.
  4. Salary gap between majors and genders.


### Technical Analysis

 **The Data**

Understanding data is a key part in analyzing and organizing data. As we're aware of the difficulty of crowd-sourcing data via API, we chose to use the `.xls` data downloaded from the [**National Center for Education Statistics**](https://nces.ed.gov/) in this project. Also, as `R` accepts `.csv` format files, we conducted several conversions so that the data would be compatible with our R codes.

**Data Wrangling**

Our goal is to visualize the number of people being conferred based on a list of majors from 2012 to 2018. In order to accomplish this goal, we have to `join` several datasets we gathered into one `dataframe` in `R`.  `Grouping` the data `by` majors would help us create bar plots to demonstrate the trend while giving users flexibility to interact with the plot on the website, such as viewing the trend for different majors or comparing trends for multiple majors. We are also interested in finding out the number of people being conferred based on ethnicities. In this case, `grouping` the data `by` different ethnicities and majors would contribute to visualizing the trends more clearly and straightforward. R also gives us opportunity to `filter` and `summarize` data by different majors and ethnicities of interests. Hence, combined with `Shiny`, users are able to find out more useful information and visualized data. 

**Libraries**

In this project, we will stick to common libraries, such as `dplyr` and `tidyr` to manipulate and reshape our primary datasets. In terms of plotting, we will utilize `plotly` as well as `ggplot2`, as these two libraries would give us more functionalities and flexibility when combining with `Shiny`. `plotly` library, in particular, would  enables interactive graphics and reactive functions for users. We will also use `shiny` library when creating website for data visualization.

**Challenges**

One of the biggest challenge in this project we anticipate is how to wrangle the datasets so that our audience can gain some insights from the data visualizations. As the data we acquired from external websites contains a lot of unexpected data and data formats, it requires a huge amount of time to logically order information, cleanup and analyze the data into the way we expect. Data visualization allows us to effectively and efficiently communicate with our audience. However, another challenge we would encounter is to allow viewers to easily navigate the information, making it easy for them to filter and compare values. Hence, for our targeted audience, especially minoritiy groups, we need to have correct and clear understandings towards their potential needs. Therefore, wrangling the data correctly would be a great challenge when we conduct our project.

Through analyzing the data, we will have a clear perception towards how the trend of people being conferred changes over time, what are some top paid majors and who tends to study them,and vise versa. This will not only gives us opportunity to understand the past, but also a way to predict the changes in the future.

# [Final Report](https://colsonxu.shinyapps.io/gender-wage-gap/)
