# Final Project Proposal

<center>
  
![](https://recruitingtimes.org/wp-content/uploads/2016/11/Gender-Pay-Gap.png)

</center>

### Project Description
- Our group will use the statistics of enrollment of each major in universities. Institute of Education Sciences gathered these statistics through surveys across
United States. The website of National Center for Education Statistics allowed me to gain access
for the data. The [**database**](https://nces.ed.gov/programs/digest/) we are using includes education information starting from 2012 to current year. More specifically, we will be looking at statistics about genders, degree types, and study areas.

- Our target audience are minorities who feel discriminated due to their gender. They believe that there are gender salary gaps.  With the data  that our team analyzes, we can find out the true reason behind gender salary gap.

- The audience of this project will, _hopefully_, learn:
  1. Are there really gender salary gaps.
  2. What is the cause of gender salary gaps.
  3. What are some top paid majors in the U.S.


### Technical Analysis

 **The Data**

Understanding data is a key part in analyzing and organizing data. As we're aware of the difficulty of crowd-sourcing data via API, we chose to use the `.xls` data downloaded from the [**National Center for Education Statistics**](https://nces.ed.gov/) in this project. Also, as `R` accepts `.csv` file, we conducted several conversions so that the data would be compatible with our R code.

**Data Wrangling**

Since our goal is to visualize the number of people being conferred based on a list of majors during a certain span of time, we have to `join` several datasets we gathered into one dataframe in `R`.  `Grouping` the data `by` majors would help us create bar plots to demonstrate the trend while giving users flexibility to interact with the plot on the website, such as viewing the trend for different majors or comparing trends from multiple majors. We are also eager to find out the number of people being conferred based on different ethnicities. In this case, we want to `group` the data `by` different ethnicities and majors. Grouping allows us to see the trends more clearly and straightforward.

**Libraries**

In this project, we will stick to the `dplyr` and   `tidyr` to manipulate and reshape our datasets. In terms of plotting, we will utilize `plotly` as well as `ggplot2`, as `plotly` would give us more functionalities and flexibility when combining `Shiny`. We will also use `shiny` library when creating website for data visualization.

**Challenges**

One of the biggest challenge in this project we anticipate is data wrangling, as the data we acquired from external websites contains a lot of useless and unexpected data. It requires a huge amount of time to cleanup and organize the files. Furthermore, the way we analyze and demonstrate data requires us to have a clear understanding of our targeted audience and their needs.

Through analyzing the data, we will have a straightforward perception towards how the trend of people being conferred changes over time, what are some top paid majors and who tends to study them, and vise versa. This will not only gives us opportunity to understand the past, but also a way to predict the changes in the future.
