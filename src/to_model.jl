using DataFrames
using ArgParse
isdefined(:Date) || using Dates
using StatsBase

function main(args)

    s = ArgParseSettings("argparse.jl: " *
                         "version info, default values, " *
                         "options with types, variable " *
                         "number of arguments.",
                         version = "Version 1.0", # version info
                         add_version = true) # auto-add version option

    @add_arg_table s begin
        "--opt1"
            nargs = '?' # '?' means optional argument
            arg_type = Any # only Int arguments allowed
            # this is used when the option is not passed
            default = "EURUSD" 
            # this is used if --opt1 is paseed with no argument
            constant = "EURUSD"
            help = "an option"
        "--karma", "-k"
            # increase a counter each time the option is given
            action = :count_invocations  
            help = "increase karma"
        "arg1"
            # eats up two arguments; puts the result in a Vector
            nargs = 2 
            help = "first argument, two " *
                   "entries at once"
            required = true
        "arg2"
            # eats up as many arguments as possible before an option
            nargs = '*' 
            # since the result will be a Vector{Any}, the default must
            # also be (or it can be [] or nothing)
            default = Any["kuku!"]
            help = "second argument, eats up " *
                   "as many items as possible " *
                   "before an option"
    end

    parsed_args = parse_args(args, s)
    println("Parsed args:")
    for (key,val) in parsed_args
        println("  $key  =>  $(repr(val))")
    end

    ticker = parsed_args["opt1"]
    year,month = parsed_args["arg1"]

    TableDate = Date(parse(Int,year), parse(Int,month))
    Days = Dates.daysinmonth(TableDate)

    DayTable = DataFrame(Act=Int32[],H=Int32[],DW=Int64[],
        WY=Int64[],SL=Int32[],TP=Int32[],AVMax=Int32[],
        BVMax=Int32[],AVSum=Int32[],BVSum=Int32[])

    for for day in 1:Days

        cd("/home/jerzy/data/csv/$ticker-$year-$month")

        try
        table = readtable("$ticker-$year-$month-$day.csv")

        if sizeof(table) == 0 continue
        end

        describe(table[:SL])
        describe(table[:TP])

        SLOpen = first(table[:SL])
        TPOpen = first(table[:TP])
        SLHigh = maximum(table[:SL])
        findmax(table[:SL]) # the max and its index
        TPHigh = maximum(table[:TP])
        findmax(table[:TP])
        AVHigh = maximum(table[:AVMax])
        BVHigh = maximum(table[:BVMax])
        SLLow = minimum(table[:SL])
        findmin(table[:SL]) # the mix and its index
        TPLow = minimum(table[:TP])
        AVTotal = sum(table[:AVSum])
        BVTotal = sum(table[:BVSum])
        MSL = median(table[:SL])
        MTP = median(table[:TP])

        if table[:Act] == 1 && table[:SL] <= MSL && table[:TP] >= MTP 
            table[:Act] = 1
        elseif table[:Act] == 1 && table[:SL] <= MSL && 
            table[:TP] >= MTP table[:Act] = 2
        else table[:Act] = 3
        end

        HourTable = DataFrame(Act = Action, H=hour, 
            DW=TrueDayOfWeek, WY=TrueWeek, 
            SL = SLPoints, TP = TPPoints, 
            AVMax = round(Integer, AskVolumeHigh),
            BVMax = round(Integer, BidVolumeHigh), 
            AVSum = round(Integer, AskVolumeTotal),
            BVSum = round(Integer, BidVolumeTotal))

        append!(DayTable, HourTable)
        catch
            day += 1
        end
    end

    if !isdir("/home/jerzy/data/csv/$ticker-$TrueYear-$TrueMonth")
        mkdir("/home/jerzy/data/csv/$ticker-$TrueYear-$TrueMonth")
    end
    cd("/home/jerzy/data/csv/$ticker-$TrueYear-$TrueMonth")
    writetable("$ticker-$TrueYear-$TrueMonth-$TrueDay.csv", DayTable)
end

main(ARGS)
