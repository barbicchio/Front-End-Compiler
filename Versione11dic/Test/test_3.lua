function void test_iterazioni()
    integer i = 0

    while i<16 do
        writeInt(i)
        i = i + 1 
    end

    repeat 
        writeInt(i)
        i = i + 1
    until i==26

    while true do
        i = i + 1
        if i > 36 then
            break
        else
            if (i/2) == 3 then
                continue
            end 
        end
        writeInt(testIfInline(i)) 
    end

    for k = 0, 14, 2 do
        writeFloat(k*3.725)
    end

    for n = i, 38, 2 do
        writeInt(n*n)
    end
end


function integer testIfInline(integer i)
    integer l = i < 0 ? -i : i
    return l
end