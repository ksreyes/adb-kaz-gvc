# ADB Kazakhstan report replication scripts

This repository contains scripts for the charts in a trade analysis report of Kazakhstan for the Asian Development Bank. Each chart is a triplet of files sharing a common file name: the chart itself, saved as a PDF in the folder [figures](figures); a CSV file containing its underlying data, saved in [data/final](data/final); and an R script that produces the two, saved in [codes](codes).

Each R script is organized into three sections.

1. **Setup.** The needed R libraries are loaded and key parameters are defined.
1. **Data.** The raw data is loaded and processed to extract the information needed for the chart. This is saved as a CSV file. 
1. **Plot.** The CSV file is loaded and (if necessary) wrangled further. The data is then plotted using the `ggplot2` library.

The Data section draws from: (1) the outputs of my other repos [adb-mrio](https://github.com/ksreyes/adb-mrio) and [baci](https://github.com/ksreyes/baci), (2) the outputs of the Python scripts in the codes folder, and (3) other publicly available datasets. The last are uploaded in [data/raw](data/raw).

In principle, everything needed to replicate a chart is found in its CSV file, which means the Data section of the script can be skipped entirely. 

## Disclaimer

This repository has no official affiliation with or approval from the Asian Development Bank.