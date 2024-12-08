#!/usr/bin/env luajit
local day08 = require("day08")
local fname = arg[1] or "input.txt"
local f = assert(io.open(fname, "r"), fname .. " missing")
local input = f:read("*a")
f:close()
local part1, part2 = day08.solve(input)
print(string.format("Part 1: %d\nPart 2: %d", part1, part2))
