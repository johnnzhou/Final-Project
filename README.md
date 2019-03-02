# Final Project Proposal

![Gender Wage Gap](https://recruitingtimes.org/wp-content/uploads/2016/11/Gender-Pay-Gap.png)
###### Copyright Recruiting Times

### Project Description
- Our group will use the statistics of enrollment of each major in universities. Institute of Education Sciences gathered these statistics through surveys across
United States. The website of National Center for Education Statistics allowed me to gain access
for the data. The [**database**](https://nces.ed.gov/programs/digest/) we are using includes education information starting from 2012 to current year. More specifically, we will be looking at statistics about genders, degree types, and study area.

- This data set can be beneficial for many people to view. However, our main target audience are minorities because they may feel discriminated due to their gender. They might believe that there are gender salary gaps. With the data that our team analyzes, we can compare different years of salaries for males and females. In addition, degree types and study areas may also provide evidence that support the gap. Using this dataset, we can help our target audience discover the real reason behind gender salary gap. 

- The audience of this project will, _hopefully_, learn:
  1. Are there really gender salary gaps.
  2. What is the cause of gender salary gaps.
  3. What are some top paid majors in the U.S.


### Technical Analysis

 **The Data**

Understanding data is a key part in analyzing and organizing data. As we're aware of the difficulty of crowd-sourcing data via API, we chose to use the `.xls` data downloaded from the [**National Center for Education Statistics**](https://nces.ed.gov/) in this project. Also, as `R` accepts `.csv` format files, we conducted several conversions so that the data would be compatible with our R codes.

**Data Wrangling**

Our goal is to visualize the number of people being conferred based on a list of majors from 2012 to 2018. In order to accomplish this goal, we have to `join` several datasets we gathered into one `dataframe` in `R`.  `Grouping` the data `by` majors would help us create bar plots to demonstrate the trend while giving users flexibility to interact with the plot on the website, such as viewing the trend for different majors or comparing trends for multiple majors. We are also interested in finding out the number of people being conferred based on ethnicities. In this case, `grouping` the data `by` different ethnicities and majors would contribute to visualizing the trends more clearly and straightforward. R also gives us opportunity to `filter` and `summarize` data by different majors and ethnicities of interests. Hence, combined with `Shiny`, users are able to find out more useful information and visualized data. 

**Libraries**

In this project, we will stick to common libraries, such as `dplyr` and   `tidyr` to manipulate and reshape our primary datasets. In terms of plotting, we will utilize `plotly` as well as `ggplot2`, as these two libraries would give us more functionalities and flexibility when combining `Shiny`, particularly `plotly` library. We will also use `shiny` library when creating website for data visualization.

**Challenges**

One of the biggest challenge in this project we anticipate is data wrangling, as the data we acquired from external websites contains a lot of unexpected data. It requires a huge amount of time to cleanup and organize the files into the way we expect. Furthermore, the way we analyze and demonstrate data requires us to have a clear understanding of our targeted audience and their needs. Therefore, wrangling the data correctly would be a great challenge when we conduct our project.

Through analyzing the data, we will have a straightforward perception towards how the trend of people being conferred changes over time, what are some top paid majors and who tends to study them, and vise versa. This will not only gives us opportunity to understand the past, but also a way to predict the changes in the future.
