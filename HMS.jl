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
    cd("/home/jerzy/calculus/data/$asset/$year/$month")
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
    dt = DateTime(Year, Month, 1, 0, 0, 0)
    StartTime = Dates.datetime2unix(dt)

    # One month of ticks is max to handle. But days of ticks have to be handle too.
    Summary = DataFrame(
        Hours = Int64[],
        Mode = Float64[],
        ScaleMode = Int64[],
        Volume = Int64[],
        Ticks = Int64[])

    for day in OpenDay:CloseDay 
        try
            for hour in 0:23 
                try

                    HourTable = DataFrame(
                        Hours = Int64[],
                        Mode = Float64[],
                        ScaleMode = Int64[],
                        Volume = Int64[], 
                        Ticks = Int64[])

                    # Sub-table for this minute.
                    t = DataFrame(Tick = Float64[], Volume = Float64[])
                    dtp = DateTime(Year, Month, day, hour, 0, 0)
                    ThisTime = Dates.datetime2unix(dtp)
                    Hours = (ThisTime - StartTime)/3600
                    #Week = Dates.week(dtp)
                    #DayOfWeek = Dates.dayofweek(dtp)
                    #DayOfWeekS = Dates.format(dtp, "e")

                    for i in 1:ticks
                        s = tab[i, :Time] 
                        if Dates.day(s) == day && Dates.hour(s) == hour
                            push!(t[:Tick], tab[i, :Ask])
                            push!(t[:Tick], tab[i, :Bid])
                            push!(t[:Volume], tab[i, :AskVolume])
                            push!(t[:Volume], tab[i, :BidVolume])
                        end
                    end

                    Mode = mode(round(t[:Tick],4))
                    Volume = sum(t[:Volume])*100
                    Ticks = nrow(t)
                    Scale = abs(round(t[:Tick],4) - Mode)
                    ScaleMode = mode(Scale)

                    HourTable = DataFrame(
                        Hours = round(Integer, Hours),
                        Mode = round(Mode,4),
                        ScaleMode = round(Integer, ScaleMode/0.0001),
                        Volume = round(Integer, Volume),
                        Ticks = Int64(Ticks))

                    append!(Summary, HourTable)

                catch
                    hour += 1
                end
            end
        catch
            day += 1
        end
    end
    nrows = nrow(Summary)
    writetable("$year-$month-HMSVT-$nrows.dat", Summary)  #month by hours
end
main(ARGS)

# https://en.wikibooks.org/wiki/Introducing_Julia/Working_with_dates_and_times
