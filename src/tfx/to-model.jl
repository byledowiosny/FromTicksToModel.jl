using DataFrames
using ArgParse
using StatsBase
#using Dates

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
            default = Any["5"]
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
    puck = parsed_args["arg2"]
    year, month = parsed_args["arg1"]

    ##################################################

    p = parse(Int, puck[1]) #default: p = 5

    # ticker = "EURUSD"
    # year = "2016"
    # month = "01"

    ##################################################
    
    cd("/home/jerzy/data")
    t = readtable("$ticker-$year-$month.csv")
    names!(t, [Symbol(i) for i in ["Asset", "Index", "Ask", "Bid"]])

    delete!(t, :Asset)

    table = DataFrame(Index = DateTime[], Ask = Float64[], Bid = Float64[])
    
    for i in 1:nrow(t)
        s = t[i, :Index] 
        a = split(s, ' ')
        b = a[1]
        y = parse(Int, b[1:4])
        m = parse(Int, b[5:6])
        d = parse(Int, b[7:8])
        c = split(a[2], '.')
        dd = split(c[1], ':')
        h = parse(Int, dd[1])
        mm = parse(Int, dd[2])
        ss = parse(Int, dd[3])
        ms = parse(Int, c[2])
        sn = DateTime(y, m, d, h, mm, ss, ms)
        push!(table[:Index], sn)
    end

    # Do the same with: 
    # sn = Dates.DateTime("$s","yyyymmdd HH:MM:SS.MS")
    
    delete!(t, :Index)
    
    table[:Ask] = t[:Ask]
    table[:Bid] = t[:Bid]

    delete!(t, :Ask)
    delete!(t, :Bid)

    # writetable("$ticker-$year-$month.csv", table)

    ##################################################

    y = parse(Int, year)
    m = parse(Int, month)
    TableDate = Date(y, m)
    Days = Dates.daysinmonth(TableDate)

    MonthTable = DataFrame(Index = DateTime[], DW = Int64[], WY = Int64[], 
            Asset = String[], AskOpen = Float64[], BidOpen = Float64[], 
            High = Float64[], Low = Float64[], TV = Int64[], 
            Puck = Int64[], Act = Int64[], SL = Int64[], TP = Int64[])

    for day in 1:Days # day = 29

        try

        for hour in 0:23 # hour = 20

            try

            tab = DataFrame(Index = DateTime[], Ask = Float64[], Bid = Float64[])
            
            for i in 1:nrow(table)
                s = table[i, :Index] 
                if Dates.day(s) == day && Dates.hour(s) == hour 
                    push!(tab[:Index], table[i, :Index])
                    push!(tab[:Ask], table[i, :Ask])
                    push!(tab[:Bid], table[i, :Bid])
                end
            end

            TableDate  = first(tab[:Index])
            AskOpen    = first(tab[:Ask])
            BidOpen    = first(tab[:Bid])
            AskHigh  = maximum(tab[:Ask])
            BidLow   = minimum(tab[:Bid])
            TicksVolume = nrow(tab)

            ticker == "XAUUSD" ? pt = 100  : pt = 100000
            ticker == "XAGUSD" ? pt = 1000 : pt = 100000
            ticker == "USDJPY" ? pt = 1000 : pt = 100000

            UpRange = 
                round(Integer, (AskHigh - AskOpen) * pt) > 0 ? 
                round(Integer, (AskHigh - AskOpen) * pt) : 1
            DownRange = 
                abs(round(Integer, (BidLow - BidOpen) * pt)) > 0 ?
                abs(round(Integer, (BidLow - BidOpen) * pt)) : 1

            RU = round(Integer, UpRange / DownRange) #Ratio Up
            RD = round(Integer, DownRange / UpRange) #Ratio Down

            if UpRange > DownRange && RU >= p 
                Action = 1; SL = DownRange; TP = UpRange
            elseif DownRange > UpRange && RD >= p 
                Action = 2; SL = UpRange; TP = DownRange
            else Action = 3; SL = 0; TP = 0
            end

            DayOfWeek = Dates.dayofweek(TableDate)
            WeekOfYear = Dates.week(TableDate)
            td = Dates.format(TableDate, "yyyy-mm-ddTHH")
            TableDate = Dates.DateTime(td)

            HourTable = DataFrame(Index = TableDate, DW = Int64(DayOfWeek), WY = Int64(WeekOfYear), 
                Asset = ticker, AskOpen = AskOpen, BidOpen = BidOpen, 
                High = AskHigh, Low = BidLow, TV = round(Integer, TicksVolume),
                Puck = p, Act = Action, SL = SL, TP = TP)

            append!(MonthTable, HourTable)

            catch
                hour += 1
            end

        end

        catch
            day += 1
        end

    end

    writetable("$ticker-$year-$month-hourly.csv", MonthTable)
    
end
main(ARGS)

# https://en.wikibooks.org/wiki/Introducing_Julia/Working_with_dates_and_times
