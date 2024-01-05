


#Edges 
cx = ax = xf = xd = ak = ky = dy = dg = yh = 1

#Vertices
set.seed(1)
C = rnorm(10000)
A = rnorm(10000)
X = rnorm(10000, cx*C + ax*A)
Fv = rnorm(10000, xf*X)
D = rnorm(10000, xd*X)
G = rnorm(10000, dg*D)
K = rnorm(10000, ak*A)
Y = rnorm(10000, ky*K + dy*D)
H = rnorm(10000, yh*Y)

#Models
summary( lm(G ~ C) ) #C and G marginally dependent
summary( lm(G ~ C + X) ) #C and G indenpendent conditioned on X 
summary( lm(G ~ C + X + Y) ) #Conditioning on collider Y opens up a different path between C and G 
summary( lm(G ~ C + X + H) ) #Conditioning on child of the collider Y (namely, H) instead of Y itself 

