# Cost and productivity effects of demographic changes on local water service

**authors:** Astrid Cullmann (Technische Universität Berlin), Caroline Stiel (DIW Berlin)

Research article published in Cullmann, A.; Stiel, C. (2022): [Cost and productivity effects of demographic changes on local water supply.](https://doi.org/10.1016/j.jup.2022.101435) Utilities Policy. 79(101435).

The empirical analysis was done in `R` and `STATA`. In this repository you find all statistical programs to conduct the analyses in the paper.

## Summary

_We investigate the impact of demographic changes on local public services taking the case of water service. We apply a structural production function approach to a large panel of German water utilities between 2003 and 2014. Exploring variation of population density and the population age structure across service areas and over time, we provide evidence that demographics and their changes significantly affect the utilities’ productivity and costs. We find that demographic changes cause significant cost increases in rapidly shrinking and ageing regions. Our results provide important policy implications regarding the prevention of growing regional disparities in public services._

## Methods and data

### Firm-level data, regional data, and geodata

 We use official microdata from the _Federal Statistical Office_ consisting of

- balance sheet [AFiD](https://gitlab.com/modern-state-owned-firms/data/afid-data) and [product data](https://gitlab.com/modern-state-owned-firms/data/public-water-supply) from German water utilities covering the years 2003-14
- as well as data on public firms in Germany [JAB](https://gitlab.com/modern-state-owned-firms/data/afid-data) and data from the [German business register](https://doi.org/10.21242/52121.2014.00.00.1.1.0)

We merge the firm-level data with 

-  data on the [German economy](https://gitlab.com/modern-state-owned-firms/data/data-german-economy)
- regional data _(Regionaldatenbank Datenbank)_ on municipality characteristics from the _Federal Statistical Office_
- geodata (shapefiles) from the _GeoBasis-DE_ data base by the _Federal Agency for Cartography and Geodesy_.

See [gitlab.com/modern-state-owned-firms/data](https://gitlab.com/modern-state-owned-firms/data) for more information on the data sources and the linkage strategy to merge all the data sources.

### Methods

We apply different statistical methods including 

- descriptive analyses
- panel data econometrics (GMM, structural estimation)
- quantile regression
- bootstrap
- hypotheses testing.

