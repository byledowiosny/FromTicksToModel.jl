# From ticks to model.

FromTicksToModel.jl provides a set of tools to transform raw ticks data (from Dukascopy Bank) to data useful for statistical modeling.

## Requirements
FromTicksToModel.jl requires `julia` to be installed with some additional packages, see `REQUIRE` file.

## Usage

j6 MCHL.jl yyyy mm  (minutely month)
j6  MMS.jl yyyy mm  (minutely month)

j6 HCHL.jl yyyy mm  (hourly month)
j6  HMS.jl yyyy mm  (hourly month)

Rscript CHL.R -f [yyyy-mm]-MCHL-[...].dat -o [yyyy-mm]-MCHLmod-[...].dat
Rscript CHL.R -f [yyyy-mm]-HCHL-[...].dat -o [yyyy-mm]-HCHLmod-[...].dat

choose -o [yyyy-mm]-M?-[...].dat -c1,? [yyyy-mm]-MCHL-[...].dat
choose -o [yyyy-mm]-MM-[...].dat -c1,2 [yyyy-mm]-MMSVT-[...].dat
choose -o [yyyy-mm]-H?-[...].dat -c1,? [yyyy-mm]-HCHL-[...].dat
choose -o [yyyy-mm]-HM-[...].dat -c1,2 [yyyy-mm]-HMSVT-[...].dat

### Unlicenced
Find the full description in the `UNLICENSE` file.

