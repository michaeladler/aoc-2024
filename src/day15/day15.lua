-- Author: Michael Adler
local M = {}

local Point2D = require("aoc_helper.point").Point2D

local dir_to_point = {
    ["^"] = Point2D:new(0, -1),
    [">"] = Point2D:new(1, 0),
    ["<"] = Point2D:new(-1, 0),
    ["v"] = Point2D:new(0, 1),
}

--- @param pos Point2D
--- @param delta Point2D
--- @return boolean
local function push_boxes(map, pos, delta)
    -- print("pushing box", pos, "into direction", delta)

    -- walk along `delta` until we encounter a non-box
    local neighbor = pos:clone()
    repeat
        neighbor = neighbor + delta
    until map[neighbor.y][neighbor.x] ~= "O"
    -- print("box which is the furthest away:", pos)

    local neighbor_value = map[neighbor.y][neighbor.x]
    if neighbor_value == "." then
        -- spot next to box is empty, so move all the boxes
        map[neighbor.y][neighbor.x] = "O"
        map[pos.y][pos.x] = "." -- we could omit this, since the caller will put the robot there
        return true
    elseif neighbor_value == "#" then
        -- print("box cannot be pushed due to wall")
        return false
    end
    error("unreachable box push")
end

--- @return Point2D
local function advance_robot(map, robot, dir)
    -- print("advancing robot", dir)
    local delta = dir_to_point[dir]
    local new_robot = robot + delta
    local value = map[new_robot.y][new_robot.x]
    if value == "#" then
        -- print("cannot move robot due to wall")
        return robot
    elseif value == "O" then
        if not push_boxes(map, new_robot, delta) then
            -- print("could not push boxes")
            return robot
        end
    end

    -- print(string.format("robot moves: old=%s, new=%s", robot, new_robot))
    map[robot.y][robot.x] = "."
    map[new_robot.y][new_robot.x] = "@"
    return new_robot
end

local function draw(map, row_count, col_count)
    print()
    for y = 1, row_count do
        for x = 1, col_count do
            io.stdout:write(map[y][x])
        end
        print()
    end
    print()
end

local function gps_sum(map, row_count, col_count)
    local result = 0
    for y = 1, row_count do
        for x = 1, col_count do
            if map[y][x] == "O" then
                result = result + (y - 1) * 100 + (x - 1)
            end
        end
    end
    return result
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

    -- process directions
    -- draw(map, row_count, col_count)
    local i, dir = 1, directions[1]
    repeat
        robot = advance_robot(map, robot, dir)
        -- draw(map, row_count, col_count)
        i = i + 1
        dir = directions[i]
    until dir == nil

    local part1 = gps_sum(map, row_count, col_count)
    return part1, 0
end

return M
