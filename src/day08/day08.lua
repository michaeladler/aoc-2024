-- Author: Michael Adler
local M = {}

---@class Point2D
---@field x number
---@field y number
local Point2D = {}
Point2D.__index = Point2D

--- @return Point2D
function Point2D.new(_, x, y)
    local instance = setmetatable({}, Point2D)
    instance.x = x or 0
    instance.y = y or 0
    return instance
end

--- @return Point2D
function Point2D.__add(a, b)
    return Point2D:new(a.x + b.x, a.y + b.y)
end

--- @return Point2D
function Point2D.__sub(a, b)
    return Point2D:new(a.x - b.x, a.y - b.y)
end

--- @return string
function Point2D:hash()
    return string.format("%d,%d", self.x, self.y)
end

function Point2D:__tostring()
    return string.format("Point2D(%d, %d)", self.x, self.y)
end

--- @param input string
M.solve = function(input)
    --- @type table<number, Point2D[]>
    local antennas = {}
    local col_count = input:find("\n") - 1
    local row_count = 0
    for line in input:gmatch("[^\r\n]+") do
        row_count = row_count + 1
        for y = 1, col_count do
            local c = string.sub(line, y, y)
            if c ~= "." then
                local t = antennas[c] or {}
                table.insert(t, Point2D:new(row_count, y))
                antennas[c] = t
            end
        end
    end

    local is_within_bounds = function(p)
        return p.x >= 1 and p.x <= col_count and p.y >= 1 and p.y <= row_count
    end

    local part1 = function()
        local antinodes = {}
        for _, points in pairs(antennas) do
            local n = #points
            for i = 1, n do
                local p = points[i]
                for j = 1, n do
                    if j ~= i then
                        local q = points[j]
                        local delta = q - p
                        local anti = q + delta
                        if is_within_bounds(anti) then
                            antinodes[anti:hash()] = true
                        end
                    end
                end
            end
        end

        local count = 0
        for _, _ in pairs(antinodes) do
            count = count + 1
        end
        return count
    end

    local part2 = function()
        local antinodes = {}
        for _, points in pairs(antennas) do
            local n = #points
            for i = 1, n do
                local p = points[i]
                for j = 1, n do
                    if j ~= i then
                        local q = points[j]
                        local delta = q - p
                        local anti = q
                        repeat
                            antinodes[anti:hash()] = true
                            anti = anti + delta
                        until not is_within_bounds(anti)
                    end
                end
            end
        end

        local count = 0
        for _, _ in pairs(antinodes) do
            count = count + 1
        end
        return count
    end

    return part1(), part2()
end

return M
