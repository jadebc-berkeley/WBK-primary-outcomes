# WBK-primary-outcomes

### WASH Benefits Kenya Primary Outcomes

This is a repository for the WASH Benefits Kenya trial [NCT01704105](https://clinicaltrials.gov/ct2/show/NCT01704105) primary analysis. The primary outcomes include length for age Z scores and diarrhea. This repository also includes scripts that compile all estimates for the primary outcomes article, including participant enrollment / loss, baseline characteristics, intervention adherence, and select secondary outcomes. This analysis was independently replicated by Jade Benjamin-Chung (jadebc@berkeley.edu) and Andrew Mertens (amertens@berkeley.edu). 

### Associated protocols and datasets

The pre-specified analysis protocol and all data required to run the analyses will be made available in concert with the publication of the article through the Open Science Framework: [https://osf.io/7urqa](https://osf.io/7urqa).

The scripts in the dm directory process raw data and create the analysis datasets that are shared publicly through OSF. The raw, unprocessed, data are not publicly avaialable at this time, but will be within approximately 2 years time (e.g., by 2019) to allow for the further development of meta-data documentation and an access platform. We will strive to update this page to link to those files when they are available.

### WASH Benefits Package

This analysis used an R package developed for the WASH Benefits study intention-to-treat analyses called [washb](https://github.com/ben-arnold/washb). 

For all analysis scripts, you will need to change directory statements within them to point them to the files on your local directory. Similar directory statement changes will be needed wherever output files are saved down (e.g., raw estimates, figures).

### Directory structure

**`analysis`** : analysis scripts

* **`1-diar`**: diarrhea analyses
* **`2-laz`**: length-for-age z-score analyses
* **`3-waz`**: weight-for-age z-score analyses
* **`4-whz`**: weight-for-height z-score analyses
* **`5-hcz`**: head circumference-for-age z-score analyses
* **`6-stunting`**: stunting analyses
* **`7-sev-stunting`**: severe stunting analyses
* **`8-wasting`**: wasting analyses
* **`9-underwt`**: underweight analyses
* **`10-btw-clus-spill`**: between-cluster spillover analyses
* **`11-negcontrol`**: negative control analyses
* **`12-mortality`**: mortality analyses

**`dm`** : data management scripts

**`figures`** : scripts to make figures

**`tables`** : scripts to make tables

