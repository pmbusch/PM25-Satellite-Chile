# Short-term exposure to fine particulate pollution and elderly mortality in Chile

Replication materials for [Busch, Rocha, Jin Lee, Cifuentes & Tai (2024)](https://www.nature.com/articles/s43247-024-01634-x).

The following code and data allows for the reproduction of all the tables, figures and calculations made in the article, both in the main body and supplementary information.

If you identify any error in the source code or have any further suggestions please contact Pablo Busch at pmbusch@ucdavis.edu.

# Organization

* **Data**: Data inputs used in the analysis. 
* **Scripts**: All code to process the data, run models and create figures. Each script starts with a description of the file purpose. Through the file there are several explanatory  comments.  
* **Figures**: Folder to store figures for publication.


# Data Sources

* **Air pollution**: Satellite data . PM2.5 estimates globally at a 0.01°x0.01° (0.01°~1.11km) resolution. Data available at yearly and monthly level. Data comes from [`Aaron van Donkelaar, Melanie S. Hammer, Liam Bindle, Michael Brauer, Jeffery R. Brook, Michael J. Garay, N. Christina Hsu, Olga V. Kalashnikova, Ralph A. Kahn, Colin Lee, Robert C. Levy, Alexei Lyapustin, Andrew M. Sayer and Randall V. Martin (2021). Monthly Global Estimates of Fine Particulate Matter and Their Uncertainty Environmental Science & Technology, 2021, doi:10.1021/acs.est.1c05309. `](https://pubs.acs.org/doi/abs/10.1021/acs.est.1c05309) Data available [`here`](https://sites.wustl.edu/acag/datasets/surface-pm2-5/)

* **Air pollution Ground data**: [`SINCA-National Air Quality Information System`](https://sinca.mma.gob.cl/)
	* Hourly and daily monitor data of PM2.5 concentrations. Multiple monitor stations with data available across Chile used to validate the satellite data.
	* Data is obtained through scripts developed from another [`project`](https://github.com/pmbusch/Reportes-SINCA)

* **Land Temperature**: [`MOD11A1.061 Terra Land Surface Temperature and Emissivity Daily Global 1km`](https://developers.google.com/earth-engine/datasets/catalog/MODIS_061_MOD11A1#bands). Daily land surface temperature data aggregated at monthly level.

* **Death Certificates**: [`Chilean Department of Statistics and Health Information`](https://deis.minsal.cl/#datosabiertos). Individual death certificates with detail on the commune of residency, sex, age, date and cause of death of the deceased.

* **Population**: [`2017 Chilean national census`](http://www.censo2017.cl/) and [`Chilean National Statistics Institute`](https://www.ine.gob.cl/estadisticas/sociales/demografia-y-vitales). National census with age, sex and spatial detail, along with 2002-2019 population interpolations.

