function integer no_ritorno()
    integer i = 1
    while i<4 do
        i = i + 1
    end
end

function void ret_void()
    return 0
end

function integer ret_err_type()
    return true
end

function void visibility()
    function void g()
        
        function void i()
            function void h()
                
            end
        
        end
    end

    function void h()
        i()
    end

end

function void test_punt1()
    integer v = 7
    pointer integer punt
    punt = _v
end


function void test_punt2(ref integer p)
    integer s = 7
    p = &s
end
