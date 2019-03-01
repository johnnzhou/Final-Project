# Final-Project Proposal


### Project Description
My group will use the dataset about education in America starting from prekindergarten through graduate school. Institute of Education Sciences gathered these statistics through surveys across 
United States. The website of National Center for Education Statistics allowed me to gain access 
for the data. Lastly, this database includes education information from 1995 to current year.

Our  target audience are minorities who feel discriminated due to their race or gender. They believe that there is still inequality in the work place.  With the data  that our team analyzes, this can prove to our audience that there still is a discrimination in the workplace due to race or gender. 


### Technical Analysis

 **The Data**
 
Understanding data is a key part in analyzing and organizing data. As we're aware of the difficulty of crowdsourcing data via API, we chose to use the `.xls` data downloaded from the [**National Center for Education Statistics**](https://nces.ed.gov/) in this project. Also, as `R` accepts `.csv` file, we conducted several conversions so that the data would be compatible with our R code. 

**Data Wrangling**

Since our goal is to visualize the number of people being conferred based on a list of majors during a certain span of time, we have to `join` several datasets we gathered into one dataframe in `R`.  `Grouping` the data `by` majors would help us create bar plots to demonstrate the trend while giving users flexibility to interact with the plot on the website, such as viewing the trend for different majors or comparing trends from multiple majors. We are also eager to find out the number of people being conferred based on different ethnicities. In this case, we want to `group` the data `by` different ethnicities and majors. Grouping allows us to see the trends more clearly and straightforward. 

**Libraries**

In this project, we will stick to the `dplyr` and   `tidyr` to manipulate and reshape our datasets. In terms of plotting, we will utilize `plotly` as well as `ggplot2`, as `plotly` would give us more functionalities and flexibility when combining `Shiny`. We will also use `shiny` library when creating website for data visualization. 

**Challenges**

One of the biggest challenge in this project that we anticipate is wrangling data, as the data we downloaded from external websites contains a lot of useless and unexpected information. This requires a huge amount of time to cleanup and organize. Furthermore, the way we analyze and demonstrate data requires us to have a clear understanding of our targeted audience and their needs.

Through analyzing the data, we will have a straightforward perception towards how the trend of people being conferred changes over time, the demographics of the graduate in different majors and ethnicities. This will not only gives us opportunity to understand the past, but also a way to predict the changes in the future.


