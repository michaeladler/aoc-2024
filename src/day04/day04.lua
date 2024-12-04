-- Author: Michael Adler
local M = {}

local function parse(input)
    local matrix, row = {}, 1
    for line in input:gmatch("[^\r\n]+") do
        matrix[row] = {}
        local i, n = 1, string.len(line)
        while i <= n do
            local c = string.sub(line, i, i)
            matrix[row][i] = c
            i = i + 1
        end
        row = row + 1
    end
    return matrix
end

M.solve = function(input)
    local part1, part2 = 0, 0
    local matrix = parse(input)
    local row_count, col_count = #matrix, #matrix[1]

    local check_east = function(row, col)
        local x, m, a, s = matrix[row][col], matrix[row][col + 1], matrix[row][col + 2], matrix[row][col + 3]
        return (x == "X" and m == "M" and a == "A" and s == "S") or (x == "S" and m == "A" and a == "M" and s == "X")
    end
    local check_south = function(row, col)
        local x, m, a, s = matrix[row][col], matrix[row + 1][col], matrix[row + 2][col], matrix[row + 3][col]
        return (x == "X" and m == "M" and a == "A" and s == "S") or (x == "S" and m == "A" and a == "M" and s == "X")
    end
    local check_se = function(row, col)
        local x, m, a, s =
            matrix[row][col], matrix[row + 1][col + 1], matrix[row + 2][col + 2], matrix[row + 3][col + 3]
        return (x == "X" and m == "M" and a == "A" and s == "S") or (x == "S" and m == "A" and a == "M" and s == "X")
    end
    local check_sw = function(row, col)
        local x, m, a, s =
            matrix[row][col], matrix[row + 1][col - 1], matrix[row + 2][col - 2], matrix[row + 3][col - 3]
        return (x == "X" and m == "M" and a == "A" and s == "S") or (x == "S" and m == "A" and a == "M" and s == "X")
    end
    matrix[row_count + 1] = {}
    matrix[row_count + 2] = {}
    matrix[row_count + 3] = {}

    for row = 1, row_count do
        for col = 1, col_count do
            if check_east(row, col) then
                part1 = part1 + 1
            end
            if check_south(row, col) then
                part1 = part1 + 1
            end
            if check_se(row, col) then
                part1 = part1 + 1
            end
            if check_sw(row, col) then
                part1 = part1 + 1
            end
        end
    end

    return part1, part2
end

return M
