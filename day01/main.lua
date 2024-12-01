#!/usr/bin/env luajit
-- Author: Michael Adler
local fname = arg[1] or "example.txt"
local f = assert(io.open(fname, "r"), fname .. " missing")
local t1, t2 = {}, {}
local count = {}
for line in f:lines() do
    local num1, num2 = line:match("(%d+)%s+(%d+)")
    local x, y = tonumber(num1), tonumber(num2)
    table.insert(t1, x)
    table.insert(t2, y)
    count[y] = (count[y] or 0) + 1
end
f:close()

table.sort(t1)
table.sort(t2)

local part1, part2 = 0, 0
for i = 1, #t1 do
    local x, y = t1[i], t2[i]
    part1 = part1 + math.abs(x - y)
    part2 = part2 + x * (count[x] or 0)
end
print(string.format("Part 1: %d\nPart 2: %d", part1, part2))
