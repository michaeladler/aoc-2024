-- Author: Michael Adler
local M = {}

--- Performs a binary search on a sorted array to find the index of a target value.
-- @param haystack table: A sorted array of values to search through.
-- @param needle any: The target value to search for.
-- @return number: The index of the target value if found, otherwise -1.
local function binary_search(haystack, needle)
    local low = 1
    local high = #haystack
    while low <= high do
        local mid = math.floor((low + high) / 2)
        if haystack[mid] == needle then
            return mid -- needle found, return its index
        elseif haystack[mid] < needle then
            low = mid + 1
        else
            high = mid - 1
        end
    end
    return -1 -- needle not found
end

M.solve = function(input)
    local part1, part2 = 0, 0

    local rules = {}
    local rules_done = false

    local lt = function(x, y)
        return binary_search(rules[x] or {}, y) ~= -1
    end

    for line in input:gmatch("[^\r\n]+") do
        if not rules_done then
            local idx = string.find(line, "|")
            if idx then
                local lhs = assert(tonumber(string.sub(line, 0, idx - 1)), "parse error")
                local rhs = assert(tonumber(string.sub(line, idx + 1)), "parse error")
                rules[lhs] = rules[lhs] or {}
                table.insert(rules[lhs], rhs)
            else
                rules_done = true
                for _, numbers in pairs(rules) do
                    table.sort(numbers)
                end
            end
        end
        if rules_done then
            local update = {}
            for s in line:gmatch("(%d+)") do
                table.insert(update, tonumber(s))
            end

            local n = #update
            local ok = true
            for i, x in ipairs(update) do
                -- check if x is smaller than all numbers after it
                for j = i + 1, n do
                    local y = update[j]
                    if not lt(x, y) then
                        ok = false
                        goto end_check
                    end
                end
            end
            ::end_check::
            local mid = math.floor((1 + #update) / 2)
            if ok then
                part1 = part1 + update[mid]
            else -- fix the update
                table.sort(update, lt)
                part2 = part2 + update[mid]
            end
        end
    end

    return part1, part2
end

return M
