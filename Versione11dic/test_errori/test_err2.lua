function void test_cond()
    while "true"  do
        integer k = 7        
    end

    if "true" then
        integer w = 5
    end
end

function void bool_expr()
    boolean q = "true" or false
    boolean w = 4.5 + true
end

function void l_expression()
    test_punt2(15)
end


function void param()
    function void n_param(integer k, boolean b)
    end

    n_param(15)
    n_param(true, 7)
    integer param = 7
end