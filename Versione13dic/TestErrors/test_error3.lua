function boolean temporary(const integer t, float vr )
    boolean return_value
    float float_value = 3.14
    if t > float_value then
      return_value = true
    else 
      return_value = false
    end
    float_value = vr
    writeFloat(vr)
    return return_value
end

function integer main ()

    boolean x
    boolean y
    integer r = 3
    float d = 3.2
    character c = 2
    x=(y or x or x or x)
    integer sasso = 8
    temporary(5, sasso)
    {} float m_0 = {2, 324.343241}
    {} integer m_1 = {2}
    
    return 0
end