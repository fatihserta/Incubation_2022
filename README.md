# Incubation_2022
Replication Data for: Elevated methane alters dissolved organic matter composition in the Arctic Ocean cold seeps 


-------------------
GENERAL INFORMATION
------------------- 


TITLE FOR THE DATASET

Replication Data for: Elevated methane alters dissolved organic matter composition in the Arctic Ocean cold seeps 

INFORMATION ON PERSON RESPONSIBLE FOR COLLECTING THE DATA

Muhammed Fatih Sert	
Department of Geosciences
UiT – The Arctic University of Norway
9037, Tromsø, Norway
Mobile: +47 92292985
E-mail: muhammed.f.sert@uit.no


DATE(S) OF DATA COLLECTION
- 2020-10 to 2020-11

GEOGRAPHIC LOCATION(S) OF DATA COLLECTION
- The Arctic Ocean; Norskebanken

LANGUAGE
- English 

DESCRIPTION

This dataset contains water column measurements, dissolved organic matter compositions, and hydrographical profiles collected from the site. 
Data contains R codes for the replication of analyses, and figures that were added in our paper.

We recommend using R Studio (v1.4) to run R project file (.Rproj) which can access all codes and data. 

Running codes in the given order will create corresponding data tables and the figures that were used in the article.
To run the codes please follow the steps given below:
1. Download the dataset in zip format and extract into a single folder (e.g. C:\...Desktop\Incubation)
2. Open 0_INCUBATION_dataset.Rproj file 
3. Click Files tab at the lower right pane and select .R code that you want to execute (e.g. 1_ctd.R)
4. Run codes of each line on the upper left pane by CTRL+Enter. Running codes in given order will import .RData and .csv files and recreate figures used in the article. 

Please see the published article for more details.  



----------------
FILE INFORMATION
----------------

1. Item List
	1. 0_INCUBATION_dataset.Rproj
	Description: R project file to manage R codes and related data 

	2. 1_ctd.R
	Description: R codes to import CTD data and execute depth profiles 

	3. 2_env_data.R
	Description: R codes to import methane, nutrient and related environmental data and execute bar plots in the manuscript

	4. 3_ms.R
	Description: R codes to import Fourier transform ion cyclotron mass spectrometry (FT-ICR MS) data for dissolved organic matter (DOM), 
	and execute multivariate analyses, barplots and van Krevelen diagrams
   
	5. inc_neg.RData
	Description: FT-ICT MS data for DOM samples with negative ionization

	6. inc_pos.RData
	Description: FT-ICT MS data for DOM samples with positive ionization

	7. methane.csv
	Description: comma-delimited file for the methane data

	8. nutrient.csv
	Description: comma-delimited file for the nutrient data

	9-10. Sta....cnv files
	Description: SeaBird cnv files for the CTD data at six hydrocast stations


