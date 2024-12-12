-- Author: Michael Adler
local M = {}

local Point2D = require("aoc_helper.point").Point2D

local north, south, east, west = Point2D:new(0, -1), Point2D:new(0, 1), Point2D:new(1, 0), Point2D:new(-1, 0)
local all_directions = { north, east, south, west } -- order matters, i and i+1 are orthogonal

local function solve_part1(map)
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

        local plant_start = map[start.y][start.x]

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
                if neighbor_plant ~= plant_start then
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
    local total = 0
    for y = 1, row_count do
        for x = 1, col_count do
            total = total + calc_price(Point2D:new(x, y))
        end
    end

    return total
end

local function solve_part2(map)
    -- key insight: number of sides = number of corners

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

        local plant_start = map[start.y][start.x]

        --- @param p Point2D
        local count_corners = function(p)
            local count = 0
            for i = 1, 4 do
                local dir1, dir2 = all_directions[i], all_directions[i % 4 + 1]
                local nb1 = p + dir1 -- e.g. north
                local nb2 = p + dir2 -- e.g. east
                local diag = p + dir1 + dir2 -- e.g. north-east (diagonal)
                local plant_nb1 = (map[nb1.y] or {})[nb1.x]
                local plant_nb2 = (map[nb2.y] or {})[nb2.x]
                local plant_diag = (map[diag.y] or {})[diag.x]
                local ext_corner = plant_nb1 ~= plant_start and plant_nb2 ~= plant_start
                local int_corner = plant_nb1 == plant_start and plant_nb2 == plant_start and plant_diag ~= plant_start
                if ext_corner or int_corner then
                    count = count + 1
                end
            end
            return count
        end

        --- @type Point2D[]
        local queue = { start }
        local area, corners = 1, count_corners(start)
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
                if neighbor_plant ~= plant_start then
                    goto continue
                end

                local candidate_hash = candidate:hash()
                if not seen[candidate_hash] then
                    seen[candidate_hash] = true
                    table.insert(queue, candidate)
                    area = area + 1
                    corners = corners + count_corners(candidate)
                end

                ::continue::
            end
        until queue[1] == nil

        return area * corners
    end

    local row_count, col_count = #map, #map[1]
    local total = 0
    for y = 1, row_count do
        for x = 1, col_count do
            total = total + calc_price(Point2D:new(x, y))
        end
    end

    return total
end

--- @param input string
M.solve = function(input)
    local map = {}
    for line in input:gmatch("[^\r\n]+") do
        local row = {}
        for i = 1, string.len(line) do
            table.insert(row, string.sub(line, i, i))
        end
        table.insert(map, row)
    end
    return solve_part1(map), solve_part2(map)
end

return M
