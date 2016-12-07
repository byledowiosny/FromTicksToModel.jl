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

    MonthTable = DataFrame(Act=Int32[], DW=Int32[], WY=Int32[], 
        DRHMax=Int32[], DR5=Int32[], DR4=Int32[], DR3=Int32[], 
        DR2=Int32[], DR1=Int32[], DRHMin=Int32[], 
        URHMin=Int32[], UR1=Int32[], UR2=Int32[], UR3=Int32[], 
        UR4=Int32[], UR5=Int32[], URHMax=Int32[], 
        AVMax=Int32[], AVHMax=Int32[], BVMax=Int32[], BVHMax=Int32[], 
        AVT=Int32[], BVT=Int32[])

    for day in 1:Days

        try
table = readtable("/home/jerzy/data/csv/$ticker-$year-$month/$ticker-$year-$month-$day.csv")

        if sizeof(table) == 0 continue
        end
        
        DR1, DR2, DR3, DR4, DR5 = quantile(table[:DR])
        DRMax, DRHMax = findmax(table[:DR]) # the max and its index
        DRMin, DRHMin = findmin(table[:DR]) # the min and its index
        UR1, UR2, UR3, UR4, UR5 = quantile(table[:UR])
        URMax, URHMax = findmax(table[:UR]) # the max and its index
        URMin, URHMin = findmin(table[:UR]) # the min and its index
        
        AVMax, AVHMax = findmax(table[:AVSum])
        BVMax, BVHMax = findmax(table[:BVSum])
        AVT = sum(table[:AVSum])
        BVT = sum(table[:BVSum])
        
        if UR3 > DR3 Action = 1
        elseif UR3 < DR3 Action = 2
        else Action = 3
        end

        DW = first(table[:DW])
        WY = first(table[:WY])

        DayTable = DataFrame(Act=Action, DW=DW, WY=WY, 
        DRHMax=DRHMax, DR5=round(Integer,DR5), DR4=round(Integer,DR4), 
        DR3=round(Integer,DR3), DR2=round(Integer,DR2), 
        DR1=round(Integer,DR1), DRHMin=DRHMin, 
        URHMin=URHMin, UR1=round(Integer,UR1), UR2=round(Integer,UR2), 
        UR3=round(Integer,UR3), UR4=round(Integer,UR4), 
        UR5=round(Integer,UR5), URHMax=URHMax, 
        AVMax=AVMax, AVHMax=AVHMax, BVMax=BVMax, BVHMax=BVHMax, 
        AVT=AVT, BVT=BVT)

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
#DRHMax = down range hour max
#DR5 = down range max
#DR4 = down range q75
#DR3 = down range median
#DR2 = down range q25
#DR1 = down range min
#DRHMin = down range hour min
#URHMin = up range hour min
#UR1 = up range min
#UR2 = up range q25
#UR3 = up range median
#UR4 = up range q75
#UR5 = up range max
#URHMax = up range hour max
#AVMax = ask volume hourly max
#AVHMax = ask volume hour of max
#BVMax = bid volume hourly max
#BVHMax = bid volume hour of max
#AVT = ask volume total of day
#BVT = bid volume total of day
