-- Author: Michael Adler
local M = {}

local Point2D = require("aoc_helper.point").Point2D

local north, south, east, west = Point2D:new(0, -1), Point2D:new(0, 1), Point2D:new(1, 0), Point2D:new(-1, 0)

local dir_to_point = {
    ["^"] = north,
    [">"] = east,
    ["<"] = west,
    ["v"] = south,
}

--- @param pos Point2D
--- @param delta Point2D
--- @return boolean
local function push_boxes(map, pos, delta)
    -- walk along `delta` until we encounter a non-box
    local neighbor = pos:clone()
    repeat
        neighbor = neighbor + delta
    until map[neighbor.y][neighbor.x] ~= "O"

    local neighbor_tile = map[neighbor.y][neighbor.x]
    if neighbor_tile == "." then
        -- spot next to box is empty, so move all the boxes
        map[neighbor.y][neighbor.x] = "O"
        map[pos.y][pos.x] = "." -- we could omit this, since the caller will put the robot there
        return true
    elseif neighbor_tile == "#" then
        return false
    end
    error("unreachable box push")
end

--- @param pos Point2D
--- @param delta Point2D
--- @return boolean
local function push_big_boxes(map, pos, delta)
    -- walk along `delta` until we encounter a non-box
    local neighbor = pos:clone()
    repeat
        neighbor = neighbor + delta
    until map[neighbor.y][neighbor.x] ~= "[" and map[neighbor.y][neighbor.x] ~= "]"

    local neighbor_tile = map[neighbor.y][neighbor.x]
    if neighbor_tile == "." then
        -- easy case: we move east or west, no stacking possible, just move all the boxes
        if delta.y == 0 then
            if delta.x == -1 then -- moving direction is west
                for x = neighbor.x, pos.x - 1, 2 do
                    map[pos.y][x] = "["
                    map[pos.y][x + 1] = "]"
                end
            else -- moving direction is east
                for x = pos.x + 1, neighbor.x, 2 do
                    map[pos.y][x] = "["
                    map[pos.y][x + 1] = "]"
                end
            end
            map[pos.y][pos.x] = "."
            return true
        end

        -- hard case: north or south
        -- 1) use bfs to determine all the boxes which we have to move
        --- @param p Point2D
        local neighbors = function(p)
            local result = {}
            local tile = map[p.y][p.x]
            if tile == "]" then
                table.insert(result, p + west)
            elseif tile == "[" then
                table.insert(result, p + east)
            end
            local vert = p + delta
            tile = map[vert.y][vert.x]
            if tile == "[" or tile == "]" then
                table.insert(result, vert)
            end
            return result
        end

        local bfs = function()
            local start = pos:clone()
            local queue = {}
            table.insert(queue, start)
            -- visited contains all the tiles we'll have to move
            local visited = { [start:hash()] = start }
            while queue[1] ~= nil do
                local head = table.remove(queue, 1)
                for _, x in ipairs(neighbors(head)) do
                    local x_hash = x:hash()
                    if not visited[x_hash] then
                        table.insert(queue, x)
                        visited[x_hash] = x
                    end
                end
            end
            return visited
        end
        local visited = bfs() -- tiles which we have to move
        --- @type Point2D[]
        local points_to_move = {} -- tiles which we have to move, in sorted (vertically) order
        for _, tile in pairs(visited) do
            table.insert(points_to_move, tile)
        end
        if delta.y == -1 then -- north, sort by smallest y
            table.sort(points_to_move, function(lhs, rhs)
                if lhs.y == rhs.y then
                    return lhs.x < rhs.x
                end
                return lhs.y < rhs.y
            end)
        else
            table.sort(points_to_move, function(lhs, rhs)
                if lhs.y == rhs.y then
                    return lhs.x < rhs.x
                end
                return lhs.y > rhs.y
            end)
        end

        -- 2) check if each tile can be moved (row by row)
        for _, p in pairs(points_to_move) do
            local dest = p + delta
            local tile = map[dest.y][dest.x]
            if tile == "." then
                goto continue
            elseif tile == "#" then -- cannot move box into a wall
                return false
            elseif tile == "[" or tile == "]" then
                -- that's bad unless we would make room here by moving the tile
                if visited[p:hash()] then
                    goto continue
                end
                return false
            end
            ::continue::
        end

        -- 3) move all boxes
        for _, p in pairs(points_to_move) do
            local dest = p + delta
            map[dest.y][dest.x] = map[p.y][p.x]
            map[p.y][p.x] = "."
        end

        return true
    elseif neighbor_tile == "#" then
        return false
    end
    error("unreachable box push")
end

--- @return Point2D
local function advance_robot(map, robot, dir)
    local delta = dir_to_point[dir]
    local new_robot = robot + delta
    local tile = map[new_robot.y][new_robot.x]
    if tile == "#" then
        return robot
    elseif tile == "O" then
        if not push_boxes(map, new_robot, delta) then
            return robot
        end
    elseif tile == "[" or tile == "]" then
        if not push_big_boxes(map, new_robot, delta) then
            return robot
        end
    end

    map[robot.y][robot.x] = "."
    map[new_robot.y][new_robot.x] = "@"
    return new_robot
end

local function gps_sum(map, row_count, col_count)
    local result = 0
    for y = 1, row_count do
        for x = 1, col_count do
            if map[y][x] == "O" or map[y][x] == "[" then
                result = result + (y - 1) * 100 + (x - 1)
            end
        end
    end
    return result
end

local function scale_map(map, row_count, col_count)
    local big_map = {}
    local robot
    for y = 1, row_count do
        big_map[y] = {}
        local big_x = 1
        for x = 1, col_count do
            local tile = map[y][x]
            if tile == "O" then
                big_map[y][big_x] = "["
                big_map[y][big_x + 1] = "]"
            elseif tile == "#" then
                big_map[y][big_x] = "#"
                big_map[y][big_x + 1] = "#"
            elseif tile == "@" then
                big_map[y][big_x] = "@"
                big_map[y][big_x + 1] = "."
                robot = Point2D:new(big_x, y)
            elseif tile == "." then
                big_map[y][big_x] = "."
                big_map[y][big_x + 1] = "."
            end
            big_x = big_x + 2
        end
    end
    return big_map, robot
end

--- @param input string
M.solve = function(input)
    local map, directions = {}, {}
    local row_count = 0
    ---@type Point2D
    local robot
    for line in input:gmatch("[^\r\n]+") do
        if string.sub(line, 1, 1) == "#" then
            row_count = row_count + 1
            map[row_count] = {}
            local i, c = 1, string.sub(line, 1, 1)
            while c ~= "" do
                map[row_count][i] = c
                if c == "@" then
                    robot = Point2D:new(row_count, i)
                end
                i = i + 1
                c = string.sub(line, i, i)
            end
        else
            local i, c = 1, string.sub(line, 1, 1)
            while c ~= "" do
                table.insert(directions, c)
                i = i + 1
                c = string.sub(line, i, i)
            end
        end
    end
    local col_count = #map[1]

    local big_map, big_robot = scale_map(map, row_count, col_count)
    local big_col_count = 2 * col_count

    local i, dir = 1, directions[1]
    repeat -- process directions
        robot = advance_robot(map, robot, dir)
        i = i + 1
        dir = directions[i]
    until dir == nil
    local part1 = gps_sum(map, row_count, col_count)

    -- part2
    i, dir = 1, directions[1]
    repeat -- process directions
        big_robot = advance_robot(big_map, big_robot, dir)
        i = i + 1
        dir = directions[i]
    until dir == nil

    local part2 = gps_sum(big_map, row_count, big_col_count)

    return part1, part2
end

return M
