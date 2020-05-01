# Microplastics-dams
The effect of dams on river transport of microplastic pollution
View the [related publication in Science of the Total Environment](https://doi.org/10.1016/j.scitotenv.2019.02.028)
    
## Contents of this repo
*****
1. `Dams_Microplastics_full.csv`: field data file (see below for metadata) <br>
2. `Dams_analysis_in_paper_clean.R`: code used to produce analysis and plots included in our paper published in *Science of the Total Environment* (2019). <br>
3. `Microplastics-dams.Rproj`: If running this locally, .RProj file will allow `.R` code to find `.csv` data file. Be sure to store `.Rproj` file in the same directory with `.R` and `.csv` to be able to run.
<br>
<br>    

## Want to use this data?
*****    
*Attribution 4.0 International*<br>
We are pleased to allow this data to be used freely in the public, with proper attribution. Please cite this data or the resulting publication.

## Notes about this project
*****
* Goal for this project: to figure out whether having a dam in the middle of a long spatial survey of a river would impact the longitudinal trends observed.
    
* Samples were collected above, within and below dams+impoundments to determine whether the presence of dams affected the concentration of microplastics in rivers.
    
* This work was presented at EGU 2019 and is published as `Watkins L, McGrattan S, Sullivan P, Walter MT. (2019). The effect of dams on river transport of microplastic pollution. Science of the Total Environment. 664: 834-840. DOI:` [10.1016/j.scitotenv.2019.02.028](https://doi.org/10.1016/j.scitotenv.2019.02.028). Please contact me if you have trouble accessing a full-text version of this publication.
<br>
<br>   

## Metadata about this project & our methods
*****
<br>     

#### Column headers of Dams_Microplastics_full.csv   

1. `DamID` the code given to each individual dam in the study.    
+ `FR` = Flatrock, Fall Creek (42.4546706,-76.4562607)    
+ `BL` = Beebe Lake, Fall Creek (42.4519266,-76.4795704)     
+ `SD` = Sediment Dam, Six Mile Creek (42.4091457,-76.4537273)    
+ `3D` = 3rd Dam, Six Mile Creek (42.4137404,-76.5299637)     
+ `2D` = 2nd Dam, Six Mile Creek (42.4247749,-76.5444474)     
+ `1D` = 1st Dam, Six Mile Creek (42.4329239,-76.4848986)
     
2. `Weight` entered **in kg** for both surface water and sediment samples. Surface water weights are calculated based on an estimated density of 1kg/L. Where for water samples, `Weight` = Sample Volume.
     
3. `nFiber`-`nBead` The count of particles of each category found via visual inspection for a given sample.

#### Site selection & Sample collection <br>
* Surface water and sediment samples collected in Fall Creek & Six Mile Creek in Ithaca, NY USA during summer 2017 by Susan McGrattan (a Cornell University undergraduate at the time).
    
* Study sites were dams located within 2 miles of each other. Their heights ranged from 5-60ft (1.5-18.5m).
    
* Sediment samples were collected where there were deposits accessible from our sampling device (a jar attatched to a long conduit). All water samples were 2L, sediment samples were 400g, wet.
    
* Water samples were 2L, sediment samples were 400g, wet weight. 
    
* Sample processing, counting and data analysis led by Susan McGrattan. [NOAA Protocols](https://marinedebris.noaa.gov/sites/default/files/publications-files/noaa_microplastics_methods_manual.pdf) were followed for sample processing of both water and sediment samples.     
  + After processing, density-separated samples were transfered via vacuum filtration onto a gridded 0.45 um filter paper for easier visual inspection. At random, 44 particles from 4/36 total samples were reexamined using 20× magnification on a WITec Alpha300R Confocal Raman Microscope using a 532nm laser at 1–2mW power.
    


