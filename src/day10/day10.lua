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

--- @return boolean
function Point2D.__eq(a, b)
    return a.x == b.x and a.y == b.y
end

--- @return string
function Point2D:hash()
    return string.format("%d,%d", self.x, self.y)
end

--- @return string
function Point2D:__tostring()
    return string.format("Point2D(%d, %d)", self.x, self.y)
end

local function contains(t, value)
    for _, v in pairs(t) do
        if v == value then
            return true
        end
    end
    return false
end

--- @param input string
M.solve = function(input)
    local part1, part2 = 0, 0
    local map = {}
    local trailheads = {}

    local y = 0
    for line in input:gmatch("[^\r\n]+") do
        y = y + 1

        local row = {}
        local x = 0
        for i = 1, string.len(line) do
            x = x + 1
            local c = tonumber(string.sub(line, i, i))
            if c == 0 then
                table.insert(trailheads, Point2D:new(x, y))
            end
            table.insert(row, c)
        end
        table.insert(map, row)
    end

    local row_count = y
    local col_count = #map[1]

    local all_directions = { Point2D:new(1, 0), Point2D:new(-1, 0), Point2D:new(0, 1), Point2D:new(0, -1) }
    --- @param p Point2D
    local neighbors = function(p)
        local height = map[p.y][p.x]
        local result = {}
        for _, d in ipairs(all_directions) do
            local candidate = p + d
            -- check out-of-bounds
            if candidate.y >= 1 and candidate.y <= row_count and candidate.x >= 1 and candidate.x <= col_count then
                if map[candidate.y][candidate.x] - height == 1 then
                    table.insert(result, candidate)
                end
            end
        end
        return result
    end

    local nines_reached = {}

    local function dfs(start, path)
        path = path or {}
        table.insert(path, start)
        if map[start.y][start.x] == 9 then
            nines_reached[start:hash()] = true
            return { path }
        end
        local all_paths = {}
        for _, neighbor in ipairs(neighbors(start)) do
            if not contains(path, neighbor) then
                local path_copy = {}
                for _, v in ipairs(path) do
                    table.insert(path_copy, v)
                end
                local new_paths = dfs(neighbor, path_copy)
                for _, new_path in pairs(new_paths) do
                    table.insert(all_paths, new_path)
                end
            end
        end
        return all_paths
    end

    for _, p in ipairs(trailheads) do
        local all_paths = dfs(p)
        for _, _ in pairs(nines_reached) do
            part1 = part1 + 1
        end
        nines_reached = {}

        part2 = part2 + #all_paths
    end

    return part1, part2
end

return M
