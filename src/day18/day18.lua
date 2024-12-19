-- Author: Michael Adler
local M = {}

local Point2D = require("aoc_helper.point").Point2D
local north, south, east, west = Point2D:new(0, -1), Point2D:new(0, 1), Point2D:new(1, 0), Point2D:new(-1, 0)

local function build_initial_map(points, map_size, iterations)
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

local function neighbors(map, current)
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

local function shortest_path(points, map_size, p1_iterations)
    local map = build_initial_map(points, map_size, p1_iterations)

    --- Dijkstra's algorithm
    local run_dijkstra = function()
        local distances = {}
        -- Initialize distances
        for y = 0, map_size do
            for x = 0, map_size do
                local p = Point2D:new(x, y)
                distances[p:hash()] = math.huge
            end
        end
        local start = Point2D:new(0, 0)
        distances[start:hash()] = 0

        -- Create priority queue
        local queue = {}
        table.insert(queue, { node = start, distance = 0 })

        while queue[1] do
            -- Extract node with minimum distance from queue
            table.sort(queue, function(a, b)
                return a.distance < b.distance
            end)
            local current = table.remove(queue, 1)

            -- Update distances for neighbors
            for _, nb in ipairs(neighbors(map, current.node)) do
                local neighbor, weight = nb.node, nb.weight
                local alt = current.distance + weight
                local neighbor_hash = neighbor:hash()
                if alt < distances[neighbor_hash] then
                    distances[neighbor_hash] = alt
                    table.insert(queue, { node = neighbor, distance = alt })
                end
            end
        end
        return distances
    end

    local dest = Point2D:new(map_size, map_size)
    local dest_hash = dest:hash()

    local distances = run_dijkstra()
    local part1 = distances[dest_hash]

    local n = #points
    local part2
    for i = p1_iterations + 1, n do
        local p = points[i]
        -- check if p blocks us from reaching our destination
        map[p.y][p.x] = "#"
        local distances_tmp = run_dijkstra()
        if distances_tmp[dest_hash] == math.huge then
            part2 = string.format("%d,%d", p.x, p.y)
            goto done
        end
    end

    ::done::
    return part1, part2
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
    local part1, part2 = shortest_path(points, 70, 1024)
    return part1, part2
end

return M
