-- Author: Michael Adler
local M = {}

local Direction = {
    NORTH = 1,
    EAST = 2,
    SOUTH = 3,
    WEST = 4,
}

local function rotate_right(dir)
    if dir == Direction.NORTH then
        return Direction.EAST
    elseif dir == Direction.EAST then
        return Direction.SOUTH
    elseif dir == Direction.SOUTH then
        return Direction.WEST
    elseif dir == Direction.WEST then
        return Direction.NORTH
    end
end

local function hash(x, y)
    return string.format("%d,%d", x, y)
end

-- The table keeps track of the number of unique keys added to it.
local function create_smart_table()
    local count = 0
    return setmetatable({}, {
        __index = function(t, k)
            return rawget(t, k)
        end,
        __newindex = function(t, k, v)
            if rawget(t, k) == nil then
                count = (count or 0) + 1
            end
            rawset(t, k, v)
        end,
        __len = function(_)
            return count
        end,
    })
end

--- @param input string
M.solve = function(input)
    local map = {}
    local row_count = 0
    local y_start, x_start
    for line in input:gmatch("[^\r\n]+") do
        row_count = row_count + 1
        local n = string.len(line)
        local row = {}
        for i = 1, n do
            local c = string.sub(line, i, i)
            if c == "^" then
                y_start = row_count
                x_start = i
            end
            table.insert(row, c)
        end
        table.insert(map, row)
    end
    local col_count = #map[1]

    local solve_part1 = function()
        local y, x = y_start, x_start
        local visited = create_smart_table()
        local dir = Direction.NORTH
        while true do
            -- mark current position as visited
            visited[hash(x, y)] = true

            ::try_again::
            -- try advance
            local new_x, new_y
            if dir == Direction.NORTH then
                new_y, new_x = y - 1, x
            elseif dir == Direction.EAST then
                new_x, new_y = x + 1, y
            elseif dir == Direction.SOUTH then
                new_y, new_x = y + 1, x
            elseif dir == Direction.WEST then
                new_x, new_y = x - 1, y
            end

            -- is OOB?
            if new_y < 1 or new_x < 1 or new_y > row_count or new_x > col_count then
                break
            end

            -- is obstacle?
            local c = map[new_y][new_x]
            if c == "#" then
                -- rotate and try again
                dir = rotate_right(dir)
                goto try_again
            end
            y, x = new_y, new_x
        end
        -- in Lua >= 5.2, we could call #visited
        return getmetatable(visited).__len()
    end

    local has_loop = function()
        local y, x = y_start, x_start
        local visited = {}
        local dir = Direction.NORTH
        while true do
            -- mark current position as visited
            local hash_code = hash(x, y)
            if visited[hash_code] == dir then
                -- found loop
                return true
            end
            visited[hash_code] = dir

            ::try_again::
            -- try advance
            local new_x, new_y
            if dir == Direction.NORTH then
                new_y, new_x = y - 1, x
            elseif dir == Direction.EAST then
                new_x, new_y = x + 1, y
            elseif dir == Direction.SOUTH then
                new_y, new_x = y + 1, x
            elseif dir == Direction.WEST then
                new_x, new_y = x - 1, y
            end

            -- is OOB?
            if new_y < 1 or new_x < 1 or new_y > row_count or new_x > col_count then
                break
            end

            -- is obstacle?
            local c = map[new_y][new_x]
            if c == "#" then
                -- rotate and try again
                dir = rotate_right(dir)
                goto try_again
            end
            y, x = new_y, new_x
        end
        return false
    end

    local solve_part2 = function()
        local count = 0
        for y = 1, row_count do
            for x = 1, col_count do
                local c = map[y][x]
                if c == "." then
                    -- try to generate a loop
                    map[y][x] = "#"
                    if has_loop() then
                        count = count + 1
                    end
                    map[y][x] = "c"
                end
            end
        end
        return count
    end

    local part1 = solve_part1()
    local part2 = solve_part2()

    return part1, part2
end

return M
