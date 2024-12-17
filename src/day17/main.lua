#!/usr/bin/env luajit
local day17 = require("day17")
local fname = arg[1] or "input.txt"
local f = assert(io.open(fname, "r"), fname .. " missing")
local input = f:read("*a")
f:close()
local part1, part2 = day17.solve(input)
print(string.format("Part 1: %s\nPart 2: %d", part1, part2))
