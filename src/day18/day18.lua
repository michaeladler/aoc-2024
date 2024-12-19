-- Author: Michael Adler
local M = {}

local Point2D = require("aoc_helper.point").Point2D
local north, south, east, west = Point2D:new(0, -1), Point2D:new(0, 1), Point2D:new(1, 0), Point2D:new(-1, 0)

local function build_map(points, map_size, iterations)
    local map = {}
    for y = 0, map_size do
        map[y] = {}
        for x = 0, map_size do
            map[y][x] = "."
        end
    end
    for i = 1, iterations do
        local p = points[i]
        map[p.y][p.x] = "#"
    end
    return map
end

local function shortest_path(points, map_size, iterations)
    local map = build_map(points, map_size, iterations)
    local neighbors = function(current)
        local result = {}
        for _, dir in ipairs({ north, east, south, west }) do
            local candidate = current + dir
            local row = map[candidate.y]
            if row and row[candidate.x] == "." then
                table.insert(result, { node = candidate, weight = 1 })
            end
        end
        return result
    end
    -- for y = 0, map_size do
    --     for x = 0, map_size do
    --         io.stdout:write(map[y][x])
    --     end
    --     print()
    -- end

    --- Dijkstra's algorithm
    local distances = {}
    -- local previous = {}
    local queue = {}

    -- Initialize distances and previous nodes
    for y = 0, map_size do
        for x = 0, map_size do
            local p = Point2D:new(x, y)
            local p_hash = p:hash()
            distances[p_hash] = math.huge
            -- previous[p_hash] = nil
        end
    end
    local start = Point2D:new(0, 0)
    distances[start:hash()] = 0

    -- Create priority queue
    table.insert(queue, { node = start, distance = 0 })

    while queue[1] do
        -- Extract node with minimum distance from queue
        table.sort(queue, function(a, b)
            return a.distance < b.distance
        end)
        local current = table.remove(queue, 1)

        -- Update distances and previous nodes for neighbors
        for _, nb in ipairs(neighbors(current.node)) do
            local neighbor, weight = nb.node, nb.weight
            local alt = current.distance + weight
            local neighbor_hash = neighbor:hash()
            if alt < distances[neighbor_hash] then
                distances[neighbor_hash] = alt
                -- previous[neighbor_hash] = current
                table.insert(queue, { node = neighbor, distance = alt })
            end
        end
    end

    local destination = Point2D:new(map_size, map_size)
    return distances[destination:hash()]
end
M.shortest_path = shortest_path

local function parse(input)
    local points = {}
    for line in input:gmatch("[^\r\n]+") do
        local x, y = line:match("(%d+),(%d+)")
        table.insert(points, Point2D:new(assert(tonumber(x)), assert(tonumber(y))))
    end
    return points
end
M.parse = parse

--- @param input string
M.solve = function(input)
    local points = parse(input)
    local part1 = shortest_path(points, 70, 1024)
    return part1, 0
end

return M
