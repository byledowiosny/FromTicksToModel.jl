using DukascopyTicksReader: DukascopyTicks, CacheDirectory,
    get_cache_dir, get_filename, get_url, get,
    to_arrays, to_dataframe, to_timearray
using DataFrames
isdefined(:Date) || using Dates
using ArgParse

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
            default = Any["no_arg_given"]
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
    day,from = parsed_args["arg2"]
    year,month = parsed_args["arg1"]

    #if day > Dates.daysinmonth(parse(Int,year), parse(Int,month))
    #    day = Dates.daysinmonth(parse(Int,year), parse(Int,month))
    #end

    TableDate = Date(parse(Int,year), parse(Int,month), parse(Int,day))
    TableDayOfWeek = Dates.dayofweek(TableDate)
    TableWeek = Dates.week(TableDate)

    cache = CacheDirectory()
    source = DukascopyTicks()

    DayTable = DataFrame(Act=Int32[],H=Int32[],DW=Int64[],
        WY=Int64[],SL=Int32[],TP=Int32[],AVMax=Int32[],
        BVMax=Int32[],AVSum=Int32[],BVSum=Int32[])

    for hour in parse(Int,from):23
        try
        reader = get(source, ticker, DateTime(parse(Int,year), 
            parse(Int,month), parse(Int,day), hour))

        if sizeof(reader) == 0 continue
        end

        table = to_dataframe(reader)
        
        AskOpen = first(table[:Ask])
        BidOpen = first(table[:Bid])
        AskHigh = maximum(table[:Ask])
        BidHigh = maximum(table[:Bid])
        AskVolumeHigh = maximum(table[:AskVolume])
        BidVolumeHigh = maximum(table[:BidVolume])
        AskLow = minimum(table[:Ask])
        BidLow = minimum(table[:Bid])
        AskVolumeTotal = sum(table[:AskVolume])
        BidVolumeTotal = sum(table[:BidVolume])

        UpRange = round(Integer, (AskHigh - AskOpen) * 100000)
        DownRange = abs(round(Integer, (BidLow - BidOpen) * 100000))

        if UpRange > DownRange Action = 1; SLPoints = DownRange; 
            TPPoints = UpRange
        elseif UpRange < DownRange Action = 2; SLPoints = UpRange; 
            TPPoints = DownRange
        end

        HourTable = DataFrame(Act = Action, H=hour, 
            DW=TableDayOfWeek, WY=TableWeek, 
            SL = SLPoints, TP = TPPoints, 
            AVMax = round(Integer, AskVolumeHigh),
            BVMax = round(Integer, BidVolumeHigh), 
            AVSum = round(Integer, AskVolumeTotal),
            BVSum = round(Integer, BidVolumeTotal))

        append!(DayTable, HourTable)
        catch
            writetable("$ticker-$year-$month-$day-$hour.csv", DayTable)
            hour += 1
        end
    end

    if !isdir("/home/jerzy/data/csv/$ticker-$year-$month")
        mkdir("/home/jerzy/data/csv/$ticker-$year-$month")
    end
    cd("/home/jerzy/data/csv/$ticker-$year-$month")
    writetable("$ticker-$year-$month-$day.csv", DayTable)
end

main(ARGS)
