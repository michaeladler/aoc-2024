-- Author: Michael Adler
local M = {}

local Point2D = require("aoc_helper.point").Point2D

local all_directions = { Point2D:new(1, 0), Point2D:new(-1, 0), Point2D:new(0, 1), Point2D:new(0, -1) }

--- @param input string
M.solve = function(input)
    local part1, part2 = 0, 0

    local map = {}
    for line in input:gmatch("[^\r\n]+") do
        local row = {}
        for i = 1, string.len(line) do
            table.insert(row, string.sub(line, i, i))
        end
        table.insert(map, row)
    end

    --- @type table<Point2D, boolean>
    local seen = {}

    ---@param start Point2D
    local calc_price = function(start) -- flood fill algorithm
        -- do not start from a region we've visited before
        local start_hash = start:hash()
        if seen[start_hash] then
            return 0
        end
        seen[start_hash] = true

        local plant = map[start.y][start.x]

        --- @type Point2D[]
        local queue = { start }
        local area, perimeter = 1, 4
        repeat
            local current = table.remove(queue, 1) -- pop
            for _, dir in ipairs(all_directions) do
                ---@type Point2D
                local candidate = current + dir

                local row = map[candidate.y]
                if not row then
                    goto continue
                end
                local neighbor_plant = row[candidate.x]
                if neighbor_plant ~= plant then
                    goto continue
                end

                perimeter = perimeter - 1
                local candidate_hash = candidate:hash()
                if not seen[candidate_hash] then
                    seen[candidate_hash] = true
                    table.insert(queue, candidate)
                    area = area + 1
                    perimeter = perimeter + 4
                end

                ::continue::
            end
        until queue[1] == nil

        return area * perimeter
    end

    local row_count, col_count = #map, #map[1]
    for y = 1, row_count do
        for x = 1, col_count do
            part1 = part1 + calc_price(Point2D:new(x, y))
        end
    end

    return part1, part2
end

return M
