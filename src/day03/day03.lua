-- Author: Michael Adler
local M = {}

M.solve = function(input)
    local part1, part2 = 0, 0
    local mul_enabled = true
    for line in input:gmatch("[^\r\n]+") do
        local n = string.len(line)
        local i = 1
        while i <= n do
            local sub = string.sub(line, i, i + 11)
            -- check for mul
            local x, y = sub:match("^mul%((%d%d?%d?),(%d%d?%d?)%)")
            if x and y then
                local prod = tonumber(x) * tonumber(y)
                part1 = part1 + prod
                if mul_enabled then
                    part2 = part2 + prod
                end
                i = i + 6
                goto continue
            end
            -- check for do
            if sub:match("^do%(%)") then
                mul_enabled = true
                i = i + 4
                goto continue
            end
            -- check for don't()
            if sub:match("^don't%(%)") then
                mul_enabled = false
                i = i + 7
                goto continue
            end
            i = i + 1
            ::continue::
        end
    end

    return part1, part2
end

return M
