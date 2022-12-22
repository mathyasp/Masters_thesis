# Masters_thesis
## Final Masters Thesis Analysis

Human disturbance is a major contributor to the degradation of ecosystems. There is a significant amount of scientific literature that analyzes the impacts of large spacial scale disturbance but very little on smaller spacial scales. Generally, it is agreed that as ecosystems are divided and dissected, they exhibit lower diversity, lower species richness, and higher invasive species rates. 

Recreational trails are an important component of human society and provide the ability for individuals to connect with nature. However, we know very little about how these trails may impact the ecosystems that they provide access to. We do know that recreational trails divide ecosystems, although the scale at which they do varies from trail to trail. 

For this study, I looked at two preserves in Southern New York to further investigate these impacts. The major differences between these parks were their land-use history and trail design, with Rockefeller State Park Preserve (RSPP) being an older preserve and a more impactful trail system consisting of gravel-covered carriage trails as opposed to Teatown Lake Reservation which is a younger preserve with a less impactful trail system composed of narrow dirt walking paths. 

 ### I analyzed 15 plot-pairs at each preserve looking at:
 
 * Species diversity
 * Tree diameter at breast height (DBH)
 * Invasive versus native plant species coverage
 * Canopy openness
 * Species richness
 
 Using the data gathered, I conducted a comprehensive data analysis using R statistical software (R Core Team, 2019), using additional packages “tidyverse”, “tidyr”, “dplyr”, "knitr" and “vegan” (Oksanen et al., 2019; Wickham, 2017; Wickham, François, Henry, & Müller, 2019; Yihui Xie, 2014; Yihui Xie 2015; Yihui Xie 2019; Wickham & Henry, 2019).
 
 ### For the analysis I used:
 
 * "ggplot2" to create boxplot visualizations
 * "kable" to create 3-way and 4-way ANOVA tables
 * "diversity" and "vegdist" from the "vegan" package in order to determine the Shannon Index values and Bray-Curtis values
 * "TukeyHSD" function from the "graphics" package in order to perform pair-wise comparisons for each measured variable and across all relevant predictor variables 
 
 The analysis of my data revealed that recreational trails did result in detectable impacts on the adjacent plant community at both preserves.
 
 ### RSPP showed:
 
 * Higher invasive coverage and species richness
 * Lower native coverage and species richness
 * Lower diversity
 * Higher turnover rate between edge and off-trail location
 
 These results would suggest that recreational trails have the potential to negatively impact an ecosystem, depending on the level of disturbance the trail causes. 
 Though more data is needed to determine the replicability of these results, it would be my recommendation for trail-builders to keep in mind the potential for harm and make efforts to limit the level of disturbance in order to protect the ecosystem.
 
![image](https://user-images.githubusercontent.com/119142489/209214098-d6b24487-4b98-4471-8cab-770e1f8023a8.png)
Figure 1: Total Percent Cover of Native and Invasive species based on Site, Location, and EcoCondition

![image](https://user-images.githubusercontent.com/119142489/209214330-374943f5-005a-4345-b635-c46fde449243.png)
Figure 2: Plant Species Richness of Native and Invasive species based on Site, Location, and EcoCondition
