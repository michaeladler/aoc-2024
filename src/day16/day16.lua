-- Author: Michael Adler
local M = {}

local Point2D = require("aoc_helper.point").Point2D
local north, south, east, west = Point2D:new(0, -1), Point2D:new(0, 1), Point2D:new(1, 0), Point2D:new(-1, 0)

--- @param current_node Point2D
--- @param orientation Point2D
local function neighbors(map, current_node, orientation)
    local result = {}

    local directions = {
        { orientation, 1 },
        { Point2D:new(-orientation.y, orientation.x), 1001 },
        { Point2D:new(orientation.y, -orientation.x), 1001 },
    }
    for _, dir in ipairs(directions) do
        local candidate = current_node + dir[1]
        if map[candidate.y] and map[candidate.y][candidate.x] ~= "#" then
            table.insert(result, { node = candidate, weight = dir[2], orientation = dir[1] })
        end
    end

    return result
end

local function hash_state(node, orientation)
    local orient_hash
    if orientation == north then
        orient_hash = "N"
    elseif orientation == south then
        orient_hash = "S"
    elseif orientation == east then
        orient_hash = "E"
    elseif orientation == west then
        orient_hash = "W"
    else
        error(string.format("invalid orientation: %s", orientation))
    end
    return string.format("%s,%s", node:hash(), orient_hash)
end

--- @param start Point2D
local function dijkstra(map, start)
    local queue = { { node = start, orientation = Point2D:new(1, 0), distance = 0 } }
    -- IMPORTANT: it's not enough to hash node! the whole state (excluding distance) must be used as key;
    -- this bug took me quite a while to find...
    local distances = { [hash_state(queue[1].node, queue[1].orientation)] = 0 } -- nil means infinity
    while queue[1] do -- while queue is not empty
        -- remove node with shortest distance
        table.sort(queue, function(a, b)
            return a.distance < b.distance
        end)

        local current = table.remove(queue, 1)

        for _, t in ipairs(neighbors(map, current.node, current.orientation)) do
            local node, weight, orientation = t.node, t.weight, t.orientation

            local alt_dist = current.distance + weight
            local node_hash = hash_state(node, orientation)
            local node_dist = distances[node_hash]
            if node_dist == nil or alt_dist < node_dist then
                distances[node_hash] = alt_dist
                table.insert(queue, { node = node, distance = alt_dist, orientation = orientation })
            end
        end
    end

    return distances
end

--- @param input string
M.solve = function(input)
    local start, destination
    local map, row_count, col_count = {}, 0, nil
    for line in input:gmatch("[^\r\n]+") do
        row_count = row_count + 1
        local row = {}
        col_count = col_count or string.len(line)
        for i = 1, col_count do
            local c = string.sub(line, i, i)
            if c == "S" then
                start = Point2D:new(i, row_count)
            elseif c == "E" then
                destination = Point2D:new(i, row_count)
            end
            table.insert(row, c)
        end
        table.insert(map, row)
    end

    local distances = dijkstra(map, start)
    local part1 = math.min(
        distances[hash_state(destination, north)] or math.huge,
        distances[hash_state(destination, east)] or math.huge,
        distances[hash_state(destination, south)] or math.huge,
        distances[hash_state(destination, west)] or math.huge
    )

    return part1, 0
end

return M
