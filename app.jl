using SingularSpectrumAnalysis
using ArgParse
using Plots
using MatrixMarket

function main(args)

    s = ArgParseSettings("argparse.jl: " *
                         "version info, default values, " *
                         "options with types, variable " *
                         "number of arguments.",
                         version = "Version 1.0", # version info
                         add_version = true) # auto-add version option

    @add_arg_table s begin
        "--asset"
            nargs = '?' #optional argument
            arg_type = Any
            default = "EURUSD" #when the asset is not passed
            constant = "EURUSD" #if --asset is paseed with no argument
            help = "an asset, if other than EURUSD"
        "--karma", "-k"
            action = :count_invocations  # increase a counter each time the option is given
            help = "increase karma"
        "y-m" # year month; a Vector
            nargs = 2
            help = "year month, first argument, two " *
                   "entries at once"
            required = true
        "day" # day; before an --asset
            nargs = '*'
            default = Any[" "] # a Vector{Any}
            help = "day, second argument, " *
                   "before an --asset"
    end

    parsed_args = parse_args(args, s)
    println("Parsed args:")
    for (key,val) in parsed_args
        println("  $key  =>  $(repr(val))")
    end

    year, month = parsed_args["y-m"] # year = "2017" month = "01"
    # day = parsed_args["day"] # day = "10"
    asset = parsed_args["asset"] # asset = "EURUSD"

    #############################################

    # Read in a file.
    cd("/home/jerzy/calculus/data/$asset/$year/$month")

    ys = readdlm("mM.dat")
    #ys = MatrixMarket.mmread("mM.mtx")
    L = 60   # Window length
    plot(ys)

    # For advanced usage, see the implementation of functions `hsvd` and `reconstruct`.
    USV = hsvd(ys,L)   # Perform svd on the trajectory matrix.
    sigmaplot(USV)     # Plot normalized singular values.
    logsigmaplot(USV)  # Plot singular values.
    cumsigmaplot(USV)  # Plot cumulative normalized singular values.
    # Determine pairs of singular values corresponding to seasonal components.
    seasonal_groupings = [1:2, 4:5]
    trends = 3         # If some singular value lacks a buddy, this is a trend component.
    pairplot(USV,seasonal_groupings)  # plot phase plots for all seasonal components
    # Reconstruct the underlying signal without noise, based on all identified
    # components with significant singular values.
    yrt, yrs = reconstruct(USV, trends, seasonal_groupings)
    yr = sum([yrt yrs],2)  # Form full reconstruction
    plot([ys yr], lab=["ys" "yr"])
    writedlm("rmM.dat", yr)

end
main(ARGS)
