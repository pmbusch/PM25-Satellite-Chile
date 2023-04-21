# PM25-Satelite-Chile
Analysis on PM2.5 data on Chile. 
Project developed for the course "STA250 - Data Science for International Development", UC Davis - Winter 2023.

# Data Sources

* **Air pollution**: Satellite data . PM2.5 estimates globally at a 0.01°x0.01° (0.01°~1.11km) resolution. Data available at yearly and monthly level. Data comes from [`Aaron van Donkelaar, Melanie S. Hammer, Liam Bindle, Michael Brauer, Jeffery R. Brook, Michael J. Garay, N. Christina Hsu, Olga V. Kalashnikova, Ralph A. Kahn, Colin Lee, Robert C. Levy, Alexei Lyapustin, Andrew M. Sayer and Randall V. Martin (2021). Monthly Global Estimates of Fine Particulate Matter and Their Uncertainty Environmental Science & Technology, 2021, doi:10.1021/acs.est.1c05309. `](https://pubs.acs.org/doi/abs/10.1021/acs.est.1c05309) Data available [here](https://sites.wustl.edu/acag/datasets/surface-pm2-5/)

* **Air pollution Ground data**: [`SINCA-National Air Quality Information System`](https://sinca.mma.gob.cl/)
	* Hourly and daily monitor data of PM2.5 concentrations. Multiple monitor stations with data available across Chile used to validate the satellite data.
	* Data is obtained through scripts developed from another [`project`](https://github.com/pmbusch/Reportes-SINCA)


# TO DO

- Literature review:
	- Papers with same approach
- Organize paper based on professor's email
- Exploratory analysis 
	- 75+ cardiovascular/respiratory
	- PM2.5 exposureUrban vs Rural
- Run models with updated data
	- Region*quarter interaction
	- Huber-White standard errors (VC robust)
- Add Income overtime (for long period analysis)
- Add temperature


# Xiao Hui Tai review

**Satellite PM2.5 validation**
- Explain your choice of the satellite imagery data set, and the data set itself, as well as the ground monitor data in a bit more detail in the main text.
- Illustrate why it is important that you independently assessed these satellite data and how others might be able to use these data now that you have validated them. - Clarify the conclusion from this part of the analysis, which has implications on the generalizability of your results to other countries and uses of this satellite data set. E.g., should others be cautious with values below and above some cutoff? This appears to be the conclusion.

- The satellite data
	- (1) is this the "leading" satellite-based data source? 
	- (2) highlight global availability at X resolution and X years in the main text (I know you have it in the Data section) 
	- (3) explain in a few sentences how the Washington University in St. Louis group derived and validated these PM2.5 estimates. 
- For the ground-based monitors
	- Should we take this to be the ground truth? What is the coverage of the ground monitors compared to raster cells? A map showing the locations of the ground-based monitors overlaid with the satellite imagery raster cells could be helpful. 
	- If you are able to find information about the cost of maintaining the network of ground monitors, or the availability of such ground monitors in other countries (particularly developing countries), that would be useful to convince the reader why using satellite imagery is advantageous. 

- Methodology
	- When did the ground-based measurements start becoming available/widespread and why did you select 50 monitors as the cutoff? Some explanation like at least one in each commune, or one covering X area, etc., could be a suitable justification. 
	- Why 24 days of validated data? How many month-locations were dropped? Does exact pixel interpolation just mean which raster cell the ground monitor is in? If you have a figure, potentially in the appendix, for how you did this bilinear interpolation, that would be helpful. 

- Conclusion
	- you state two hypotheses at the bottom of Page 3. There appears to be some confounding between low PM2.5, being in the north, having fewer monitors (related: how few is few?) and monitors not being well-calibrated or maintained because air pollution is less of a concern. Is there a way of figuring out what the specific issue is? E.g., to disentangle low PM2.5 values or fewer monitors, maybe you could look at the South where there are more monitors, during the summer when pollution is low. If satellite measurements are also not accurate, this tells us that the issue is with the satellite imagery in the low PM2.5 ranges (which I think is your conclusion).
	- Other than region and year, it might be useful to do the correlation analysis by season. 

- Why is below 12 considered low and > 50 high? Some explanation of the distribution (e.g., X% of observations below 12%, or an explanation of WHO guidelines) would help the audience make sense of these values. If we exclude the extreme values that are not measured well, what is the correlation? 

**The second part of the analysis**
- Emphasize that estimating these causal effects would not be possible without the use of this satellite imagery data set. 
- Highlight how what you learned in the first part of the analysis can be applied in the second. E.g., you removed some regions with bad data as a robustness check. If the conclusion is more about the extreme ranges than the specific regions, how about removing the months or locations without these observations? This would make the link between the first and second research questions more explicit and demonstrate how others can use this information in the same way. 

- Why is the model with the region-quarter interaction not your preferred model? region-quarter fixed effects would at least partially take care of the effect of temperature. If you include region-quarter fixed effects, do you still see the heterogeneity in estimates for North, Center and South? The center has high levels of pollution and so a larger effect might make sense, but why is the effect in the North so high? 
- More on fixed effects: it would be good to see if the results still hold after including additional fixed effects. Have you tried using month instead of just quarter? Quarter-year? 

- Lagged effects: 
	- Think about the mechanisms in which such monthly fluctuations cause death. Is it that when pollution reaches particularly high levels, people with existing conditions tend to die? Some medical or public health background, if available, would be useful here to cite. 
	- Longer term effects of pollution might be captured by the commune fixed effects (or if you include commune-year fixed effects, these might be better).
	- For lagged effects, are there then shorter-term effects that we would expect from exposure in the previous month? Two months before? E.g., is there some disease that results in death a month later? 

- Other questions about methodology and results: 
	- I am a bit concerned with this 16% rural population being dropped in the analysis. Is this only for the PM2.5 population exposure estimation? Does the all-cause mortality include the rural population? 
	- Why is the coefficient estimate for the Fall quarter so much higher? 
	- The average 75+ mortality is increasing over time. Is this because the denominator is 2017 Census data, while the above 75+ population is actually increasing? 

- Highlight at least one heterogeneity result that is interesting (in your abstract etc., as a contribution). If you have a good explanation for the North, Center and South result, this could be a good one. A bit more of an explanation on why these breakdowns are interesting to you and what you might expect to see, whether your results support these, and the public health implications, would also be interesting. 

**Discussion**
- Some more discussion about the 5% increase in mortality risk would be helpful. How does this compare to wealthy countries in particular? Should this number be comparable with those that you cited in the literature? If so, this is higher than for all ages (?) at 4% in the US (Yitshak-Sade, 2019) and lower than 14% (25+). Why?  - Include more public health implications. What can be done now that we have these estimates?
- Number of avoided deaths: 
	- If you do a careful analysis, I think this can go into the abstract as a main takeaway as well. You mention that it is overestimated because it should be measured as an annual average.
	- Can you construct different scenarios for the monthly numbers that would make sense, such that the yearly average is 12 or 20? E.g., use the seasonal pattern and introduce one with minimal fluctuations and one with large fluctuations. I'm not quite sure I understood the part about "the dynamic effect of avoiding deaths over the whole period." You highlight in the conclusion that this work is particularly important for the central/south part of the country, and so in these hypothetical scenarios, if you calculate numbers of avoidable deaths for these regions that would be particularly convincing. 

4/21 meeting
mortality EDA 
another heft-neal paper (earlier one) individual level infant mort, dhs survey, panel with fixed effect 
wealth effect 
include temperature and square of it
try to put it as many fixed effects as possible 
model with lags and leads
still effect if cardiopulmonary and respiratory 
pm2.5 measurement issue: one paper saying satellite inaccurate for biomass burning area
review PNAS sustainability science paper 



