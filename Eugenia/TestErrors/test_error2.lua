function integer main () 
    {}{} integer numbers = {{1,2},{3,4},{5,6},{7,8}}
    integer i = 0

    {4} boolean positive


    while i < 4 do
        positive{i} = isPositive(numbers{i}, 2)
        i++
    end

    pointer integer es_point


    return 0
end

--[commento di prova


function boolean isPositive ({}integer array, integer dim)
    integer i = 0
    while i < dim do
        if array{i} <= 0 then 
            return false
        end
        i++
    end
    return true
end