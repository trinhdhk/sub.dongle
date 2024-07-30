# sub.dongle: An R dongle for subgroup tidying and plotting

This is the repository for the dongle that extend functionality to autimatically perform subgroup analyses.
This package mainly contains two functions:

## Tidy sub-group models

`tidy_subgroup` relies on `tidy` method for any model that is fed into. As long as there is such method, the function can perform well.

## Forest plot for the tidied subgroup models.

`forestploter` uses `forestploter::forest` to nicely convert tidied subgroup summary into a forest plot. Users can post-process the dataset and add more stats into forest plot.

## TO-DO

May be implementing a method for `forestplot::forest_plot`.
