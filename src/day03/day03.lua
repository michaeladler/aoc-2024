-- Author: Michael Adler
local M = {}

M.solve = function(input)
	local part1, part2 = 0, 0
	for line in input:gmatch("[^\r\n]+") do
		for x, y in line:gmatch("mul%((%d%d?%d?),(%d%d?%d?)%)") do
			x, y = tonumber(x), tonumber(y)
			part1 = part1 + x * y
		end
	end
	return part1, part2
end

return M
