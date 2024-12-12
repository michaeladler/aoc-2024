local M = {}

---@class Point2D
---@field x number
---@field y number
local Point2D = {}
M.Point2D = Point2D
Point2D.__index = Point2D

--- @return Point2D
--- @param x number
--- @param y number
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

return M
