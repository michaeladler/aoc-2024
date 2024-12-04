-- Author: Michael Adler
local M = {}

--- @param input string
M.solve = function(input)
    local part1, part2 = 0, 0
    for line in input:gmatch("[^\r\n]+") do
        -- good luck
        print(line)
    end
    return part1, part2
end

return M
