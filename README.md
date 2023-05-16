# MOTUS_R

Code to download tag detections from MOTUS server using R, clean up data to inspect specific tags, and trilateration code to estimate locations of birds detected in node array. 

There are two files:
  - MOTUS_Download.R downloads the data from MOTUS based on project ID number. Can download all tag data (all detections of your owned tags) or all your detections (from your own stations, all tags that went through the area). Includes a trilateration function that requires adjustment based on the function you fit from a known-tag walk. 
  - MOTUS_Calibration.R uses data collected from a tag-calibration walk to fit a function for trilateration. Takes known locations from a GPS file (.csv) and uses signal strength from tag detections to estimate the relationship between RSSI and distance. The function you fit in this code can then be used to estimate locations of tags within your receiver area. Note that this code assumes raw data is coming from CTT website - will need to be adjusted slightly if using data downloaded from MOTUS because column names don't exactly match between the two sources. 

Trilateration code based on Paxton et. al (2022), Optimizing trilateration estimates for tracking fine‐scale movement of wildlife using automated radio telemetry networks. https://github.com/kpaxton75/EcolEvol.Manuscript_Optimizing.Trilateration 
 
