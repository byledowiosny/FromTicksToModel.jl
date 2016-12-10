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
            default = Int32[5]
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
    puck = parsed_args["arg2"]
    p = puck[1]

    TableDate = Date(parse(Int,year), parse(Int,month))
    Days = Dates.daysinmonth(TableDate)

    MonthTable = DataFrame(D = DateTime[], 
        DW = Int32[], WY = Int32[], 
        Asset = ASCIIString[], 
        AskOpen = Float64[], BidOpen = Float64[], 
        High = Float64[], Low = Float64[], 
        AVT = Int32[], BVT = Int32[], 
        Puck = Int32[], Act = Int32[],
        SL = Int32[], TP = Int32[])

    for day in 1:Days

        try
            cd("/home/jerzy/data/csv/$ticker-$year-$month")
            table = readtable("$ticker-$year-$month-$day.csv")

            if sizeof(table) == 0 continue
            end
        
            TableDate = first(table[:D])
            DW = first(table[:DW])
            WY = first(table[:WY])
            AskOpen = first(table[:AskOpen])
            BidOpen = first(table[:BidOpen])
            High = maximum(table[:High])
            Low = minimum(table[:Low])
            #DR1, DR2, DR3, DR4, DR5 = quantile(table[:DR])
            #UR1, UR2, UR3, UR4, UR5 = quantile(table[:UR])
            AVT = sum(table[:AVT])
            BVT = sum(table[:BVT])

            UpRange = 
                round(Integer, (High - AskOpen) * 100000) > 0 ? 
                round(Integer, (High - AskOpen) * 100000) : 1
            DownRange = 
                abs(round(Integer, (Low - BidOpen) * 100000)) > 0 ?
                abs(round(Integer, (Low - BidOpen) * 100000)) : 1

            RU = round(Integer, UpRange / DownRange) #Ratio Up
            RD = round(Integer, DownRange / UpRange) #Ratio Down

            if UpRange > DownRange && RU >= p 
                Action = 1; SL = DownRange; TP = UpRange
            elseif DownRange > UpRange && RD >= p 
                Action = 2; SL = UpRange; TP = DownRange
            else Action = 3; SL = 0; TP = 0
            end

            DayTable = DataFrame(D = DateTime(TableDate), 
                DW = DW, WY = WY, 
                Asset = ticker, 
                AskOpen = AskOpen, BidOpen = BidOpen, 
                High = High, Low = Low, 
                AVT = AVT, BVT = BVT, 
                Puck = p, Act = Action,
                SL = SL, TP = TP)

            append!(MonthTable, DayTable)
        catch
            day += 1
        end
    end
    if !isdir("/home/jerzy/data/csv/$ticker-$year")
        mkdir("/home/jerzy/data/csv/$ticker-$year")
    end
    cd("/home/jerzy/data/csv/$ticker-$year")
    writetable("$ticker-$year-$month.csv", MonthTable)
end
main(ARGS)

#describe(table[:SL])
#describe(table[:TP])
#QDR = nquantile(table[:DR], 8)
#QUR = nquantile(table[:UR], 8)

#Act = Action: 1 buy, 2 sell, 3 do nothing
#DW = day of week
#WY = week of year 
#DR5 = down range max
#DR4 = down range q75
#DR3 = down range median
#DR2 = down range q25
#DR1 = down range min
#UR1 = up range min
#UR2 = up range q25
#UR3 = up range median
#UR4 = up range q75
#UR5 = up range max
#AVT = ask volume total of day
#BVT = bid volume total of day
