float i=5
float a=1
integer b=2
float c=a+b+7
--[const integer vero=10
integer resto=32
float carlo=50.0
integer arco=32
pointer integer point= &(b)

function integer restrepo(ref integer k,valres float r,res integer pluto,integer orla)
pluto=31
--[vero++ --[ok controllato dal typechecker
point=&pluto
pluto++
k++
k=b+32
r=k-r
r=41.3
return pluto
end

function void main() 
b=restrepo(resto,carlo,arco,b)
end