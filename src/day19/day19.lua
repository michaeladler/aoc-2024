-- Author: Michael Adler
local M = {}

local function parse(input)
    local available, designs = {}, {}
    local is_first = false
    for line in input:gmatch("[^\r\n]+") do
        if not is_first then
            for pat in line:gmatch("(%l+)") do
                table.insert(available, pat)
            end
            is_first = true
        else
            table.insert(designs, line)
        end
    end
    return available, designs
end

local function has_prefix(str, prefix)
    return string.sub(str, 1, string.len(prefix)) == prefix
end

--- @param input string
M.solve = function(input)
    local available, designs = parse(input)

    local cache = {}

    --- @param design string
    local function total_solutions(design)
        if design == "" then
            return 1
        end
        local cached_result = cache[design]
        if cached_result then
            return cached_result
        end

        local result = 0

        for _, pattern in ipairs(available) do
            if has_prefix(design, pattern) then -- split off the prefix and recurse
                local rest = string.sub(design, 1 + string.len(pattern))
                result = result + total_solutions(rest)
            end
        end

        cache[design] = result
        return result
    end

    local part1, part2 = 0, 0
    for _, design in ipairs(designs) do
        local count = total_solutions(design)
        if count > 0 then
            part1 = part1 + 1
            part2 = part2 + count
        end
    end
    return part1, part2
end

return M
