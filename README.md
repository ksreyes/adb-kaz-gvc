# Replication scripts for the 2023 ADB Kazakhstan GVC Report

This repository contains the scripts behind the charts in the 2023 Asian Development Bank (ADB) publication *Kazakhstan's Resource Economy: Diversification Through Global Value Chains*. Each chart is a triplet of files sharing a common filename: the chart itself, saved as a PDF in the folder [figures](figures); a csv file that depicts its underlying data, saved in [data/final](data/final); and an R script that produces the two, saved in [codes](codes).

Each R script is organized into three sections.

1. **Setup.** The needed R libraries are loaded and key parameters are defined.
1. **Data.** The raw data is loaded and processed to extract the information needed for the chart. This is saved as a csv file.
1. **Plot.** The csv file is loaded and (if necessary) wrangled further. The data is then plotted using the `ggplot2` library.

The Data section draws from the outputs of the Python scripts in the codes folder, the outputs of my other repos [adb-mrio](https://github.com/ksreyes/adb-mrio) and [baci](https://github.com/ksreyes/baci), and other publicly available datasets. The last are uploaded in [data/raw](data/raw).

In principle, everything needed to replicate a chart is found in its csv file, which means the Data section of the script can be skipped entirely. 

## Gallery

![](/figures/2.1_growth.pdf)

## Citing

```bibtex
@misc{ksreyes2023adbkazgvc,
    title = {{Replication scripts for the 2023 ADB Kazakhstan GVC report}},
    author = {{Kenneth S. Reyes}},
    url = {https://github.com/ksreyes/adb-kaz-gvc},
    year = {2023},
}
```

## Disclaimer

The contents of this repository are in no way endorsed by the Asian Development Bank or the authors of cited works. All errors are my own.
