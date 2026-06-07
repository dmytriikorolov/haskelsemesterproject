x = 2
y = 5
println(x + y * 3)

if y > x
    println(true)
else
    println(false)
end

i = 1
sum = 0
while i <= 4
    sum = sum + i
    i = i + 1
end
println(sum)

for k = 1:3
    println(k * k)
end

println(let a = 10
    b = 20
    max(a, b)
end)
