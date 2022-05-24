
## RSRS Special Issue: Innovation in Regional Graphics 

### *An Exploration of Regional Retail Visits during the COVID-19 Pandemic*

**Authors**: Patrick Ballantyne, Alex Singleton, Les Dolega
**Paper**: https://www.tandfonline.com/doi/full/10.1080/21681376.2021.1973548

### Introduction 

This repo contains all the code used to produce the main visualisation for this submission to this special issue in Regional Studies, Regional Science. 

"*Regional graphics can be incredibly powerful tools for telling stories and making arguments about the state of regions and regionalism. This special issue is a compilation of some of the best examples of the format. We are inviting scholars from any discipline to contribute visualisations (maps, infographics, charts, or other forms of spatial visualisations) that address topics conceptualised at the regional scale.*" 

For more info on the special issue, see the call for papers [HERE](https://www.regionalstudies.org/special_issue/rsrs-innovation-in-regional-graphics/).

In our submission we adopt a regional approach, focusing on the Chicago Metropolitan Statistical Area (MSA) as a case study, to explore changes to retail visits during the early weeks of the COVID-19 pandemic. Using data from [SafeGraph](https://www.safegraph.com/covid-19-data-consortium#:~:text=SafeGraph%20is%20providing%20free%20access,COVID%2D19%20(Coronavirus)), we explore total visits to retail places for a six-week period during the 'first peak' of the pandemic (02/03 - 06/04). The focus of this Regional Graphic is on mapping weekly change in retail visits at H3 across the Chicago MSA (seen below), but we also disaggregate these visits by Retail Type to explore the response in visits to 'essential' retail places (convenience) in comparison to leisure-based retail, which you can view [HERE](Outputs/barchart.tiff). The code used to produce these visualisations can be found [HERE](RSRS_Graphic.R).

### Graphic

  <p align="center">
 <img width="500" height="650" src ="Outputs/Maps/RSRS.gif">
</p>



### Acknowledgements

* Thanks to SafeGraph for providing access to their various datasets through their COVID-19 data consortium.
* Thanks also to the developers of the packages we have used here - ['tmap'](https://github.com/mtennekes/tmap) (Martijn Tennekes and Team), ['magick'](https://github.com/ropensci/magick) (Jeroen Ooms), ['ceramic'](https://cran.r-project.org/web/packages/ceramic/index.html) (Michael Sumner and Team) and ['h3jsr'](https://github.com/obrl-soil/h3jsr) (Lauren O'Brien).
* Thanks also to my co-authors for useful guidance and feedback on the submission.

