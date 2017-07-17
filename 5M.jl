using DataFrames
using ArgParse

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

    # Read in a file of ticks.
    cd("/home/jerzy/data/$asset-monthly")
    table = readtable("Ticks$year$month.csv")

    ticks = nrow(table)

    # New table with true dates.
    tab = DataFrame(Time = DateTime[], Ask = Float64[], Bid = Float64[],
                    AskVolume = Float64[], BidVolume = Float64[])
    
    # The strings have to be transform to dates.
    formate = "y-m-d H:M:S.s"
    for i in 1:ticks
        s = table[i, :Time_UTC_] 
        sn = Dates.DateTime(s, formate; locale="english")
        push!(tab[:Time], sn)
        push!(tab[:Ask], table[i, :Ask])
        push!(tab[:Bid], table[i, :Bid])
        push!(tab[:AskVolume], table[i, :AskVolume])
        push!(tab[:BidVolume], table[i, :BidVolume])
    end

    # For automatic handling of records, first and last dates are needed.
    OpenDate  = first(tab[:Time])
    CloseDate  = last(tab[:Time])
    OpenDay = Dates.day(OpenDate)
    CloseDay = Dates.day(CloseDate)

    # Some variables are needed outside of loops.
    Year = Dates.year(OpenDate)
    Month = Dates.month(OpenDate)
    #Week = Dates.week(OpenDate)
    #DayOfWeek = Dates.dayofweek(OpenDate)
    DayOfWeekS = Dates.format(OpenDate, "e")

    # One month of ticks is max to handle. But days of ticks have to be handle too.
    Summary = DataFrame(
        Year = Int64[], Month = Int64[], Day = Int64[],
        DOW = String[], 
        Hour = Int64[], Minute = Int64[],
        Close = Float64[], 
        High = Float64[], Low = Float64[], Mode = Float64[],
        RangeH = Int64[], RangeL = Int64[],
        Volume = Int64[], Ticks = Int64[])

    for day in OpenDay:CloseDay 
        try
            for hour in 0:23 
                try
                    for minute in 0:59 
                    try

                    MinuteTable = DataFrame(
                        Year = Int64[], Month = Int64[], Day = Int64[],
                        DOW = String[], 
                        Hour = Int64[], Minute = Int64[],
                        Close = Float64[], 
                        High = Float64[], Low = Float64[], Mode = Float64[],
                        RangeH = Int64[], RangeL = Int64[],
                        Volume = Int64[], Ticks = Int64[])

                    # Sub-table for this minute.
                    t = DataFrame(Tick = Float64[], Volume = Float64[])

                    for i in 1:ticks
                        s = tab[i, :Time] 
                        if Dates.day(s) == day && Dates.hour(s) == hour && Dates.minute(s) == minute
                            push!(t[:Tick], tab[i, :Ask])
                            push!(t[:Tick], tab[i, :Bid])
                            push!(t[:Volume], tab[i, :AskVolume])
                            push!(t[:Volume], tab[i, :BidVolume])
                        end
                    end

                    Close = last(round(t[:Tick],4))
                    High = maximum(round(t[:Tick],4))
                    Low = minimum(round(t[:Tick],4))
                    Mode = mode(round(t[:Tick],4))
                    Volume = sum(t[:Volume])*100
                    # And finaly, the number of ticks by minute.
                    Ticks = nrow(t)
                    RangeL = (Low - Mode)/0.0001
                    RangeH = (High - Mode)/0.0001

                    MinuteTable = DataFrame(
                        Year = Int64(Year), Month = Int64(Month), Day = Int64(day),
                        DOW = String(DayOfWeekS),
                        Hour = Int64(hour), Minute = Int64(minute),
                        Close = round(Close,4),
                        High = round(High,4), Low = round(Low,4), Mode = round(Mode,4),
                        RangeH = round(Integer, RangeH),
                        RangeL = round(Integer, RangeL),
                        Volume = round(Integer, Volume), Ticks = Int64(Ticks))

                    append!(Summary, MinuteTable)

                    catch
                        minute += 1
                    end
                    end
                catch
                    hour += 1
                end
            end
        catch
            day += 1
        end
    end
    writetable("5M$year$month.dat", Summary)
end
main(ARGS)

# https://en.wikibooks.org/wiki/Introducing_Julia/Working_with_dates_and_times
