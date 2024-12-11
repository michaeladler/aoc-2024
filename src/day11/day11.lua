-- Author: Michael Adler
local M = {}

local function get_digits(num)
    local result = {}
    while num > 0 do
        table.insert(result, 1, num % 10)
        num = math.floor(num / 10)
    end
    return result
end

local function blink(stones)
    local new_stones = {}
    for value, count in pairs(stones) do
        -- Rule 1: If the stone is engraved with the number 0, it is replaced by a stone
        -- engraved with the number 1.
        if value == 0 and count > 0 then
            new_stones[1] = (new_stones[1] or 0) + count
            goto continue
        end

        -- Rule 2: If the stone is engraved with a number that has an even number of
        -- digits, it is replaced by two stones. The left half of the digits are
        -- engraved on the new left stone, and the right half of the digits are
        -- engraved on the new right stone.
        local digits = get_digits(value)
        local n = #digits
        if n % 2 == 0 then
            local left = 0
            local mul = 1
            for i = n / 2, 1, -1 do
                left = left + digits[i] * mul
                mul = mul * 10
            end

            local right = 0
            mul = 1
            for i = n, n / 2 + 1, -1 do
                right = right + digits[i] * mul
                mul = mul * 10
            end

            new_stones[left] = (new_stones[left] or 0) + count
            new_stones[right] = (new_stones[right] or 0) + count
            goto continue
        end

        -- If none of the other rules apply, the stone is replaced by a new
        -- stone; the old stone's number multiplied by 2024 is engraved on the
        -- new stone.
        local new_value = value * 2024
        new_stones[new_value] = (new_stones[new_value] or 0) + count

        ::continue::
    end
    return new_stones
end

--- @param input string
M.solve = function(input)
    local stones = {}
    for s in input:gmatch("%d+") do
        stones[assert(tonumber(s), "value must not be nil")] = 1
    end

    local part1, part2 = 0, 0
    for i = 1, 75 do
        stones = blink(stones)
        if i == 25 then
            for _, count in pairs(stones) do
                part1 = part1 + count
            end
        end
    end

    for _, count in pairs(stones) do
        part2 = part2 + count
    end

    return part1, part2
end

return M
