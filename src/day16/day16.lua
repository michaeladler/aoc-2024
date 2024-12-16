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
        { Point2D:new(-orientation.y, orientation.x), 1001 }, -- counter-clockwise
        { Point2D:new(orientation.y, -orientation.x), 1001 }, -- clockwise
    }
    for _, dir in ipairs(directions) do
        local candidate = current_node + dir[1]
        local row = map[candidate.y]
        if row and row[candidate.x] ~= "#" then
            table.insert(result, { node = candidate, weight = dir[2], orientation = dir[1] })
        end
    end

    return result
end

local function hash_orient(orientation)
    if orientation == north then
        return "N"
    elseif orientation == south then
        return "S"
    elseif orientation == east then
        return "E"
    elseif orientation == west then
        return "W"
    end
    error(string.format("invalid orientation: %s", orientation))
end

local function hash_state(node, orientation)
    return string.format("%s,%s", node:hash(), hash_orient(orientation))
end

--- Performs a shallow clone of the table t.
--- @param t table
--- @return table
local function clone(t)
    local copy = {}
    for k, v in pairs(t) do
        copy[k] = v
    end
    return copy
end

--- @param start Point2D
local function dijkstra(map, start, destination)
    local part1 = math.huge

    local queue = { { node = start, orientation = Point2D:new(1, 0), distance = 0, path = { [start:hash()] = true } } }
    -- IMPORTANT: it's not enough to hash node! the whole state (excluding distance) must be used as key;
    -- this bug took me quite a while to find...
    local distances = { [hash_state(queue[1].node, queue[1].orientation)] = 0 } -- nil means infinity
    local all_paths = {}
    while queue[1] do -- while queue is not empty
        -- remove node with shortest distance
        table.sort(queue, function(a, b)
            return a.distance < b.distance
        end)

        local current = table.remove(queue, 1)
        if current.node == destination and current.distance <= part1 then
            -- copy all points from path
            for k, _ in pairs(current.path) do
                all_paths[k] = true
            end
            -- NOTE: we may end up reaching our destination multiple times (from different neighbor tiles)
            part1 = current.distance
        end

        for _, t in ipairs(neighbors(map, current.node, current.orientation)) do
            local node, weight, orientation = t.node, t.weight, t.orientation

            local alt_dist = current.distance + weight
            local node_hash = hash_state(node, orientation)
            local node_dist = distances[node_hash]
            -- NOTE: <= is important here, otherwise we don't find all best paths
            if node_dist == nil or alt_dist <= node_dist then
                distances[node_hash] = alt_dist
                local new_path = clone(current.path)
                new_path[node:hash()] = true
                table.insert(queue, { node = node, distance = alt_dist, orientation = orientation, path = new_path })
            end
        end
    end

    local part2 = 0
    for _, _ in pairs(all_paths) do
        part2 = part2 + 1
    end

    return part1, part2
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
    local part1, part2 = dijkstra(map, start, destination)
    return part1, part2
end

return M
