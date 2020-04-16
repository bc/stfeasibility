sampling from a log-concave; gaussian; a bit more general 
 - take logarithm of the density to get a concave distribution
uniform: is a special case
 - assumes that all points are equally likely


 lipschitz constraint

 the lipschitz norm has to be less than \delta

 	finds the value of highest delta for a given muscle
the outliers can be expressed as the lipshitz norm:

|x2 - x1|
lp norm is the approach Norm(x, p=2)
lipschitz_norm <- f() max(abs(x2 -x1),abs(x3 -x2),... abs(x7 -x6))

4x7 per moment
7 moments

49 dimensional subspace to sample from

A_1 x_1 <= b_1
0 < a_i < 1

A_1 x_1 <= b_1
A_2 x_2 <= b_2
A_3 x_3 <= b_3
0 < a_i < 1


Product distribution
 - this is what you get when you append many polytopes together into higher dimensions


A_1 x_1 <= b_1
A_2 x_2 <= b_2
A_3 x_3 <= b_3
0 < a_i < 1

spatiotemporal constraints
|muscle 1 in moment 2 - muscle1 in moment 1| < \delta

in case where theta is 1: becomes a redundant constraint. not a degenerate case'; not-further-constrained case

reason why we can't sample ind:
there's a 
it's a property of high dimensional spaces
picking a point on a ball, very likely to pick something very very close to the surface.

31 muscles
6 dimensions of out wrench

3,3,3,  (len 31)
'each of these situations are unlikely. when you take a product they become very very unlikely.'

n=7 muscles
num tasks = 7
x \in [0,1]n*numtasks
HAR --> x

x_0
does your seed point affect downstream distributions?
how does a seed point affect the polytope's structure

A_1 x_1 <= b
x_1 == (0.5,0.5,0.5,...) # this is the stationary measure
A_i x_i <= b_i
lipschitz constraints
x \in [0,1]

what is the downstream probability?
the stationary measure you'd assign to the starting point.
It's a small probabbility, but it is a positive one

task, number of points it took to get to 100 valid solution, p
x_2,s98 more p) 

constrained_set <- intersection of the box from the given point, with (polytope at i + 1)

Get volume of the constrained set / polytope at i + 1

Ben Cousins

relationship of volumes



https://link.springer.com/article/10.1007/s12532-015-0097-z