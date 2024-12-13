-- Author: Michael Adler
local M = {}

local Point2D = require("aoc_helper.point").Point2D

--- Calculate m, n such that:
---
--- (1) a*m + b*n = e
--- (2) c*m + d*n = f
---
local function solve_2x2(a, b, c, d, e, f)
    local det = a * d - b * c
    if det == 0 then
        return nil, nil -- no solution exists
    end
    local m = (e * d - b * f) / det
    local n = (a * f - e * c) / det
    return m, n
end

--- @param game Point2D[]
local function play_game(game)
    local a_button, b_button, prize = game[1], game[2], game[3]
    -- solve equations for m, n:
    -- a_button.x * m + b_button.x * n = prize.x
    -- a_button.y * m + b_button.y * n = prize.y
    local m, n = solve_2x2(a_button.x, b_button.x, a_button.y, b_button.y, prize.x, prize.y)
    if m and n and m % 1 == 0 and n % 1 == 0 then
        return 3 * m + n
    end
    return nil
end

--- @param input string
M.solve = function(input)
    local part1, part2 = 0, 0

    local games = {}

    local btn_a
    local btn_b
    for line in input:gmatch("[^\r\n]+") do
        local c, x, y = line:match("^Button ([AB]): X%+(%d+), Y%+(%d+)")
        if c then
            x, y = assert(tonumber(x)), assert(tonumber(y))
            if c == "A" then
                btn_a = Point2D:new(x, y)
            else
                btn_b = Point2D:new(x, y)
            end
            goto continue
        end
        x, y = line:match("^Prize: X=(%d+), Y=(%d+)")
        if x and y then
            x, y = assert(tonumber(x)), assert(tonumber(y))
            local prize = Point2D:new(x, y)
            table.insert(games, { btn_a, btn_b, prize })
            -- reset
            btn_a, btn_b = nil, nil
        end

        ::continue::
    end

    for _, game in pairs(games) do
        local cost = play_game(game)
        if cost then
            part1 = part1 + cost
        end
    end

    return part1, part2
end

return M
