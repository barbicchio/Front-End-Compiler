function integer no_ritorno()
    integer i = 1
    while i<4 do
        i++
    end
end

function void ret_void()
    return 0
end

function integer ret_err_type()
    return true
end

function void error_array()
    {3}{2}integer mat = {{1,2,3},{4,5,6},{7,8,9}}
    {}{}integer mat_2 = {1,2,3,4,5,6}
end

function {2}integer return_array()
    return {1,2,3,4} 
end

function void const_assign(const integer k)
    k = 15
    --k
    ++k
    k--
    k++
end

function void err_assign()
    integer a = 6.78
    integer b = 'c'
    integer c = "ciao"
    boolean d = "true"
    boolean e = 't'
    boolean f = 1
    string g = 4
    string h = 'c'
    string i = prova
    string l = true
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

