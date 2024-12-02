#!/usr/bin/env luajit
local day02 = require("day02")
local fname = arg[1] or "input.txt"
local f = assert(io.open(fname, "r"), fname .. " missing")
local input = f:read("*a")
f:close()
local part1, part2 = day02.solve(input)
print(string.format("Part 1: %d\nPart 2: %d", part1, part2))
