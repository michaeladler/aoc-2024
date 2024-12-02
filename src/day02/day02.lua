-- Author: Michael Adler
local M = {}

local function is_safe(numbers)
    local n = #numbers
    local has_inc, has_dec = false, false
    for i = 1, n - 1 do
        local delta = numbers[i] - numbers[i + 1]
        if delta > 0 then
            has_inc = true
            if has_dec then
                return false
            end
        elseif delta < 0 then
            has_dec = true
            if has_inc then
                return false
            end
        else -- delta is zero
            return false
        end

        local abs_delta = math.abs(delta)
        if abs_delta < 1 or abs_delta > 3 then
            return false
        end
    end
    return true
end

M.solve = function(input)
    local part1, part2 = 0, 0
    for line in input:gmatch("[^\r\n]+") do
        local numbers = {}
        for num in line:gmatch("%d+") do
            table.insert(numbers, tonumber(num))
        end
        if is_safe(numbers) then
            part1 = part1 + 1
            part2 = part2 + 1
        else -- can we make it safe?
            local n = #numbers
            for idx_skip = 1, n do
                local candidates = {}
                for j = 1, n do
                    if j ~= idx_skip then
                        table.insert(candidates, numbers[j])
                    end
                end
                if is_safe(candidates) then
                    part2 = part2 + 1
                    goto next
                end
            end
        end
        ::next::
    end
    return part1, part2
end

return M
