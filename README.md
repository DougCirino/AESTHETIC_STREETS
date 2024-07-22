# AESTHETICS_STREETS

Research compendium to reproduce analyses of the article:

Cirino D, Mouquet N, Metzger JP (2024) It is not only about green: modeling streetscapes aesthetic in a megacity. In prep.

---

## General
This repository is structured as follows:

- **data/**: contains data required to reproduce figures and tables
- **R/**: contains R functions developed for this project
- **analyses/**: contains R scripts to run specific analyses
  - `Features_Analysis.R`: code to analyze the features of the 420 street images used in the study
  - `Social_characteristis_test.R`: analyzes the effect of different social characteristics of the respondents in relation to the evaluation of the images on the survey
  - `Elo_scores2.R`: calculates the Elo's scores for each image, assuming that there is no difference between social characteristics
  - `Modeling_final.R`: statistical analysis between landscape, image features, and the scores of the images
  - `correlation_GVI_Aesthetics.R`: analysis of the correlation between green view index and aesthetics
  - `GVI_Aesthetics_MAP.R`: produces a biplot map between GVI and aesthetics - Fig. 8A
  - `supply_demand.R`: analysis of the supply of the aesthetical E.S. and the local demand. Produces the biplot between supply and demand - Fig. 5C
  - `Google_streets.R`: get the Google Street View images using the Google API and the R package googleway used for further extrapolation of the Elo scoring
- **results/**: follows the structure of analyses. Contains intermediate results and the numeric results used to produce the figures.

---

## Storage

All the 420 images used for the survey and for the final statistical modeling are available on Flickr: [https://www.flickr.com/photos/197958479@N08/albums/72177720306842197](https://www.flickr.com/photos/197958479@N08/albums/72177720306842197)  
All the images were resized and treated by the script `analysis/Images/Images.R`

---

## Images Coordinates and Final Table with Modeling Information

The coordinates of each image are present in `data/dataBase_FINAL.csv`, with other information used for the modeling, such as the figure features and the landscape metrics for each image/landscape.

---

## Figures
Figures and Tables will be stored in figures_tables/.

The following Figures and Tables can be reproduced with the script indicated in brackets (all in analyses/):
 - Figure 3 (`Modeling_final.R`)
 - Figure 4 (`Modeling_final.R`)
 - Figure 5c (`supply_demand.R`)
 - Figure 7 (`Social_characteristis_test.R`) -  needs to download and threat the images from flicker
 - Figure 8a (`GVI_Aesthetics_MAP.R`)
 - Figures S1 to S11 (`Social_characteristis_test.R`) - needs to download and threat the images from flicker
 - Figures S12 and S13 (`Modeling_final.R`)
 - Figures S14 to S16 (`Features_Analysis.R`)
 - Figure S17 (`correlation_GVI_Aesthetics.R`)
 - Figure S18 (`Social_characteristis_test.R`)
 - Table S1 to Table S4 (`Modeling_final.R`)
 - Figure S19 (`Elo_scores2.R`)
