# PM25-Satelite-Chile

PM2.5 satellite air pollution and elderly mortality risk: A case study from Chile

Project developed for the course "STA250 - Data Science for International Development", UC Davis - Winter 2023. Assistant Professor Xiao Hui Tai.
Pablo Busch, Paulo Rocha and Kyung Jin Lee

# Data Sources

* **Air pollution**: Satellite data . PM2.5 estimates globally at a 0.01°x0.01° (0.01°~1.11km) resolution. Data available at yearly and monthly level. Data comes from [`Aaron van Donkelaar, Melanie S. Hammer, Liam Bindle, Michael Brauer, Jeffery R. Brook, Michael J. Garay, N. Christina Hsu, Olga V. Kalashnikova, Ralph A. Kahn, Colin Lee, Robert C. Levy, Alexei Lyapustin, Andrew M. Sayer and Randall V. Martin (2021). Monthly Global Estimates of Fine Particulate Matter and Their Uncertainty Environmental Science & Technology, 2021, doi:10.1021/acs.est.1c05309. `](https://pubs.acs.org/doi/abs/10.1021/acs.est.1c05309) Data available [`here`](https://sites.wustl.edu/acag/datasets/surface-pm2-5/)

* **Air pollution Ground data**: [`SINCA-National Air Quality Information System`](https://sinca.mma.gob.cl/)
	* Hourly and daily monitor data of PM2.5 concentrations. Multiple monitor stations with data available across Chile used to validate the satellite data.
	* Data is obtained through scripts developed from another [`project`](https://github.com/pmbusch/Reportes-SINCA)

* **Land Temperature**: [`MOD11A1.061 Terra Land Surface Temperature and Emissivity Daily Global 1km`](https://developers.google.com/earth-engine/datasets/catalog/MODIS_061_MOD11A1#bands). Daily land surface temperature data aggregated at monthly level.

* **Death Certificates**: [`Chilean Department of Statistics and Health Information`](https://deis.minsal.cl/#datosabiertos). Individual death certificates with detail on the commune of residency, sex, age, date and cause of death of the deceased.

* **Population**: [`2017 Chilean national census`](http://www.censo2017.cl/) and [`Chilean National Statistics Institute`](https://www.ine.gob.cl/estadisticas/sociales/demografia-y-vitales). National census with age, sex and spatial detail, along with 2002-2019 population interpolations.

# TO DO

**Pablo:**

- Footnote for Metropolitan - this is the name of the region
- Figures of PM2.5 vs income, educ level, ... (each commune a dot, with pop as size)
- Change model base form from Year+Quarter to Year:Quarter (identical results, but more robust)
- Figure 3: Split it into different panels for thematic: Heterogeneity (RM vs rest), Adaptation (income, time) and Robustness (sex, without some obs.)
- Mortality scenarios at the end - reduce PM2.5 proportionally in a year to achieve 20, or reduce only in winter months?
- New model without outliers and model with PDA sampling
- Think about the mechanisms in which such monthly fluctuations cause death. Is it that when pollution reaches particularly high levels, people with existing conditions tend to die? Some medical or public health background, if available, would be useful here to cite.
    - Longer term effects of pollution might be captured by the commune fixed effects (or if you include commune-year fixed effects, these might be better).
    - For lagged effects, are there then shorter-term effects that we would expect from exposure in the previous month? Two months before? E.g., is there some disease that results in death a month later?

**Paulo**:

- Map figures including monitor data. + maps for metropolitan region
- Move monitor part to appendix
    - Explain your choice of the satellite imagery data set, and the data set itself, as well as the ground monitor data in a bit more detail in the main text.
    - explain in a few sentences how the Washington University in St. Louis group derived and validated these PM2.5 estimates.
    - why using satellite imagery is advantageous.
    - Emphasize that estimating these causal effects would not be possible without the use of this satellite imagery data set.

**Jin**:

- Literature review and organize introduction + background
- See functional forms for PM2.5
- Discussion SECTION: Some more discussion about the 2% increase in mortality risk would be helpful. How does this compare to wealthy countries in particular? Should this number be comparable with those that you cited in the literature? If so, this is LOWER than for all ages (?) at 4% in the US (Yitshak-Sade, 2019) and lower than 14% (25+). Why? - Include more public health implications. What can be done now that we have these estimates?
