-- Author: Michael Adler
local M = {}

local Point2D = require("aoc_helper.point").Point2D

-- local function draw(robots, width, height)
--     local map = {}
--     for _, rbt in pairs(robots) do
--         local row = map[rbt.y] or {}
--         row[rbt.x] = (row[rbt.x] or 0) + 1
--         map[rbt.y] = row
--     end
--
--     for y = 0, height - 1 do
--         for x = 0, width - 1 do
--             local count = (map[y] or {})[x]
--             if count then
--                 io.stdout:write(tostring(count))
--             else
--                 io.stdout:write(".")
--             end
--         end
--         io.stdout:write("\n")
--     end
-- end

--- @param input string
M.solve = function(input, width, height)
    width, height = width or 101, height or 103

    local robots = {}
    local velos = {}
    for line in input:gmatch("[^\r\n]+") do
        local rbt
        local vel
        for x, y in line:gmatch("([-]?%d+),([-]?%d+)") do
            local p = Point2D:new(assert(tonumber(x)), assert(tonumber(y)))
            if rbt then
                vel = p
            else
                rbt = p
            end
        end
        table.insert(robots, rbt)
        table.insert(velos, vel)
    end

    local advance = function()
        for i, rbt in ipairs(robots) do
            local vel = velos[i]
            rbt.x = (rbt.x + vel.x) % width
            rbt.y = (rbt.y + vel.y) % height
        end
    end

    for _ = 1, 100 do
        advance()
    end

    -- count robots in quadrants
    local mid_height = math.floor(height / 2)
    local mid_width = math.floor(width / 2)
    local top_left, top_right, lower_left, lower_right = 0, 0, 0, 0
    for _, rbt in ipairs(robots) do
        local x, y = rbt.x, rbt.y
        if x < mid_width and y < mid_height then
            top_left = top_left + 1
        elseif x > mid_width and y < mid_height then
            top_right = top_right + 1
        elseif x < mid_width and y > mid_height then
            lower_left = lower_left + 1
        elseif x > mid_width and y > mid_height then
            lower_right = lower_right + 1
        end
    end

    local part1 = top_left * top_right * lower_left * lower_right

    local part2 = 0

    return part1, part2
end

return M
