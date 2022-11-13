import math


z_x_a = 3.030303030303
z_x_g = 3.9431119712451
z_x_h = 5.08474576185

y_a = 5.81
y_g = 4.536430315
y_h = 3.542030981

deltas = [
	abs(z_x_a - y_a),
	abs(z_x_g - y_g),
	abs(z_x_a - y_g),
	abs(z_x_g - y_a),
	abs(z_x_h - y_a),
	abs(z_x_a - y_h),
	abs(z_x_h - y_h),
	abs(z_x_h - y_g),
	abs(z_x_g - y_h),
]

print('DELTAs: ', deltas)

min_delta = min(deltas)

for i in range(len(deltas)):
	if deltas[i] == min_delta:
		print('MIN DELTA NUM: ', i+1)

xs = [1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0]
ys = [9.44, 5.16, 4.43, 3.31, 3.48, 3.2, 2.34, 2.13, 2.18]

n = 9

sum1 = sum([math.log(xi)**2 for xi in xs])
sum2 = sum([math.log(xi) for xi in xs])
sum3 = sum([1/yi for yi in ys])
sum4 = sum([math.log(xi)/yi for (xi, yi) in zip(xs, ys)])

a = (sum4-sum3*sum2/n)/((n*sum1-sum2**2)/n)
b = (sum3-a*sum2)/n

print('a = ', a)
print('b = ', b)

func = lambda x: 1/(a*math.log(x)+b)

z_ys = [func(xi) for xi in xs]

sd = sum([(z_yi - yi)**2 for (z_yi, yi) in zip(z_ys, ys)])

print('sd = ', sd)


