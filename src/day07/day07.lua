-- Author: Michael Adler
local M = {}

local function has_solution(target, numbers)
    local n = #numbers
    if n == 1 then
        return target == numbers[1]
    end

    local x, y = numbers[1], numbers[2]
    if x > target then
        return false
    end

    local remaining_numbers = { x + y }
    for i = 3, n do
        table.insert(remaining_numbers, numbers[i])
    end
    if has_solution(target, remaining_numbers) then
        return true
    end

    remaining_numbers[1] = x * y
    if has_solution(target, remaining_numbers) then
        return true
    end
    return false
end

local function concat_numbers(x, y)
    local tmp = y
    while tmp > 0 do
        x = x * 10
        tmp = math.floor(tmp / 10)
    end
    return x + y
end
M.concat_numbers = concat_numbers

local function has_solution_2(target, numbers)
    local n = #numbers
    if n == 1 then
        return target == numbers[1]
    end

    local x, y = numbers[1], numbers[2]
    if x > target then
        return false
    end

    local remaining_numbers = { x + y }
    for i = 3, n do
        table.insert(remaining_numbers, numbers[i])
    end
    if has_solution_2(target, remaining_numbers) then
        return true
    end

    remaining_numbers[1] = x * y
    if has_solution_2(target, remaining_numbers) then
        return true
    end

    remaining_numbers[1] = concat_numbers(x, y)
    if has_solution_2(target, remaining_numbers) then
        return true
    end
    return false
end

--- @param input string
M.solve = function(input)
    local part1, part2 = 0, 0
    for line in input:gmatch("[^\r\n]+") do
        local target
        local numbers = {}
        for num in line:gmatch("%d+") do
            if not target then
                target = tonumber(num)
            else
                table.insert(numbers, tonumber(num))
            end
        end
        if has_solution(target, numbers) then
            part1 = part1 + target
            part2 = part2 + target
        elseif has_solution_2(target, numbers) then
            part2 = part2 + target
        end
    end
    return part1, part2
end

return M
