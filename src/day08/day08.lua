-- Author: Michael Adler
local M = {}

local Point2D = require("point2d")
local pprint = require("pprint")

--- @param input string
M.solve = function(input)
    local matrix = {}
    --- @type table<number, Point2D[]>
    local antennas = {}
    local col_count
    local row_count = 0
    for line in input:gmatch("[^\r\n]+") do
        row_count = row_count + 1
        if not col_count then
            col_count = string.len(line)
        end
        local row = {}
        for y = 1, col_count do
            local c = string.sub(line, y, y)
            if c:match("[0-9A-Za-z]") then
                local t = antennas[c] or {}
                table.insert(t, Point2D:new(row_count, y))
                antennas[c] = t
            end
            table.insert(row, c)
        end
        table.insert(matrix, row)
    end

    local is_within_bounds = function(p)
        return p.x >= 1 and p.x <= col_count and p.y >= 1 and p.y <= row_count
    end

    local antinodes = {}
    for name, points in pairs(antennas) do
        local n = #points
        for i = 1, n do
            local p = points[i]
            for j = 1, n do
                if j ~= i then
                    local q = points[j]
                    local delta = q - p
                    local anti = q + delta
                    if is_within_bounds(anti) then
                        antinodes[anti:hash()] = true
                    end
                end
            end
        end
    end

    local part1, part2 = 0, 0
    for k, _ in pairs(antinodes) do
        part1 = part1 + 1
    end

    return part1, part2
end

return M
